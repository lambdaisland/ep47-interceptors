(ns lambdaisland.interceptor
  "An interceptor implementation for educational purposes.

  Interceptors form a versatile pattern for modelling processes, in particular
  request/response type event handling such as HTTP requests or GUI events.

  For real-world implementations have a look at [pedestal.interceptor](http://pedestal.io/guides/what-is-an-interceptor), [re-frame interceptors](https://github.com/Day8/re-frame/blob/master/docs/Interceptors.md), and [Sieppari](https://github.com/metosin/sieppari).

  Features deliberately ommitted from this implementation:

  - async support
  - binding
  - terminators")

(defn- throwable->ex-info [t interceptor stage]
  (ex-info (str "Exception in interceptor " (:name interceptor) " during the " stage " stage.")
           (merge
            {:stage stage
             :interceptor (:name interceptor)
             :exception-type (keyword (pr-str (type t)))
             :exception t}
            (ex-data t))))

(defn- try-stage [stage interceptor context & args]
  (if-let [f (get interceptor stage)]
    (try
      (apply f context args)
      (catch Throwable t
        (assoc context ::error (throwable->ex-info t interceptor stage))))
    context))

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

(defn exit-1
  "Invoke the `:exit` stage of the next interceptor.

  Pop the next interceptor off the stack, and run its `:exit` stage if it has
  one."
  [{::keys [stack] :as context}]
  (let [interceptor (peek stack)
        new-stack   (pop stack)
        new-context (assoc context ::stack new-stack)]
    (try-stage :exit interceptor new-context)))

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

(defn process-exit
  "Process the `:exit` stage of all interceptors on the stack.

  Run through all interceptors on the stack, executing their `:exit` stage and
  popping them off the stack one by one, until the stack is empty or an error
  occured."
  [{::keys [stack] :as context}]
  (if (seq stack)
    (let [new-context (exit-1 context)]
      (if (::error new-context)
        new-context
        (recur new-context)))
    context))

(defn process-error
  "Process the `:exit` stage of all interceptors on the stack.

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
  (update-in context [::queue]
             (fnil into clojure.lang.PersistentQueue/EMPTY)
             interceptors))

(defn execute
  "Execute the context.

  This assumes interceptors have been enqueued with [[enqueue]]. It will run
  through the `:enter` stage of all interceptors in the queue, and then through
  their `:exit` stage in reverse order.

  If at some point an exception is raised, then it will skip the rest of the
  `:enter` or `:exit` stage, and instead run the `:error` stage of all
  interceptors that still had to exit."
  [context]
  (-> context
      process-enter
      process-exit
      process-error))



#_
(-> {}
    (enqueue [{:name ::error-handler
               :error (fn [ctx e]
                        (prn "Got error in " (:interceptor (ex-data e)))
                        (dissoc ctx ::error e))}
              {:name ::i1
               :enter #(update % :counter (fnil inc))
               :exit #(update % :counter (fnil + 0) 10)}
              {:name ::i2
               :enter #(update % :counter (fnil + 0) 100)
               :exit #(update % :counter (fnil + 0) 1000)}])
    enter-1
    enter-1)
