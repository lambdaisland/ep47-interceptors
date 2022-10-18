(ns lambdaisland.interceptor)

(defn- throwable->ex-info [t interceptor stage]
  (ex-info (str "Exception in interceptor " (:name interceptor) " during the " stage " stage.")
           (merge
            {:stage stage
             :interceptor (:name interceptor)
             :exception-type (keyword (pr-str (type t)))
             :exception t}
            (ex-data t))))

(defn- try-stage
  "Try running a specific stage of the given interceptor.

  Will catch exceptions and switch the context over to error handling by
  removing the `::queue` and adding an `::error` key."
  [stage interceptor context & args]
  (if-let [f (get interceptor stage)]
    (try
      (apply f context args)
      (catch :default t
        (-> context
            (dissoc ::queue)
            (assoc ::error (throwable->ex-info t interceptor stage)))))
    context))

(defn into-queue
  "Add elements to a queue, setting up a new queue if no queue was provided."
  ([xs]
   (into-queue nil xs))
  ([q xs]
   ((fnil into cljs.core.PersistentQueue/EMPTY) q xs)))

(defn enter-1
  "Invoke the `:enter` stage of the next interceptor.

  Pop the next interceptor off the queue, push it onto the stack, and run its
  `:enter` stage if it has one. "
  [{::keys [queue stack] :as context}]
  (let [interceptor (peek queue)
        new-queue   (pop queue)
        new-stack   (conj stack interceptor)
        new-context (assoc context ::queue new-queue ::stack new-stack)]
    (try-stage :enter interceptor new-context)))

(defn leave-1
  "Invoke the `:leave` stage of the next interceptor.

  Pop the next interceptor off the stack, and run its `:leave` stage if it has
  one."
  [{::keys [stack] :as context}]
  (let [interceptor (peek stack)
        new-stack   (pop stack)
        new-context (assoc context ::stack new-stack)]
    (try-stage :leave interceptor new-context)))

(defn error-1
  "Invoke the `:error` stage of the next interceptor.

  Pop the next interceptor off the stack, and run its `:enter` stage if it has
  one."
  [{::keys [stack error] :as context}]
  (let [interceptor (peek stack)
        new-stack   (pop stack)
        new-context (assoc context ::stack new-stack)]
    (try-stage :error interceptor new-context error)))

(defn process-enter
  "Process the `:enter` stage of all interceptors on the queue.

  Run through all interceptors on the queue, executing their `:enter` stage and
  moving them from the queue to the stack, until the queue is empty or an error
  occured."
  [{::keys [queue] :as context}]
  (if (seq queue)
    (let [new-context (enter-1 context)]
      (if (::error new-context)
        new-context
        (recur new-context)))
    context))

(defn process-leave
  "Process the `:leave` stage of all interceptors on the stack.

  Run through all interceptors on the stack, executing their `:leave` stage and
  popping them off the stack one by one, until the stack is empty or an error
  occured."
  [{::keys [stack] :as context}]
  (if (seq stack)
    (let [new-context (leave-1 context)]
      (if (::error new-context)
        new-context
        (recur new-context)))
    context))

(defn process-error
  "Process the `:leave` stage of all interceptors on the stack.

  If the context does not contain an `::error` then this is a no-op.

  Run through all interceptors on the stack, executing their `:error` stage and
  popping them off the stack one by one, until the stack is empty."
  [{::keys [error stack] :as context}]
  (if (and error (seq stack))
    (recur (error-1 context)) context))

(defn enqueue
  "Enqueue interceptors.

  Add interceptors to the context's FIFO queue."
  [context interceptors]
  (update-in context [::queue] into-queue interceptors))

(defn execute
  "Execute the context.

  This assumes interceptors have been enqueued with [[enqueue]]. It will run
  through the `:enter` stage of all interceptors in the queue, and then through
  their `:leave` stage in reverse order.

  If at some point an exception is raised, then it will skip the rest of the
  `:enter` or `:leave` stage, and instead run the `:error` stage of all
  interceptors that still had to leave."
  [context]
  (-> context
      process-enter
      process-leave
      process-error))

(defn ctx-summary
  "Take a context map, but rewrite the queue, stack and error to be more concise
  for easy inspection."
  [{::keys [queue stack error] :as context}]
  (cond-> (dissoc context ::queue ::stack ::error)
    (some? queue)
    (assoc :queue (into-queue (map :name queue)))
    (some? stack)
    (assoc :stack (map :name stack))
    (some? error)
    (assoc :error (dissoc (ex-data error) :exception))))
