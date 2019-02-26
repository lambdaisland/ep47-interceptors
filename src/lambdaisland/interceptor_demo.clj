(ns lambdaisland.interceptor-demo
  (:require [lambdaisland.interceptor :refer [enqueue
                                              execute
                                              process-enter
                                              process-error
                                              process-leave
                                              enter-1
                                              leave-1
                                              error-1
                                              ctx-summary]]))

(-> {:count 0}
    (enqueue [{:name :add-1
               :enter (fn [ctx]
                        (update ctx :count + 1))}
              {:name :add-10
               :enter (fn [ctx]
                        (update ctx :count + 10))}])
    :lambdaisland.interceptor/queue
    type)
;; => clojure.lang.PersistentQueue




(-> clojure.lang.PersistentQueue/EMPTY
    (conj :a) ;; => <-(:a)-<
    (conj :b) ;; => <-(:a :b)-<
    (conj :c) ;; => <-(:a :b :c)-<
    (pop)     ;; => <-(:b :c)-<
    (conj :d) ;; => <-(:b :c :d)-<
    (conj :e) ;; => <-(:b :c :d :e)-<
    (pop)     ;; => <-(:c :d :e)-<
    (pop)     ;; => <-(:d :e)-<
    (pop)     ;; => <-(:e)-<
    (pop)     ;; => <-()-<
    )




(defn make-interceptor [x y]
  {:name  (keyword (str "add-" x "-then-" y))
   :enter (fn [ctx]
            (update ctx :count + x))
   :leave (fn [ctx]
            (update ctx :count + y))})


(-> {:count 0}
    (enqueue [(make-interceptor 1 2)
              (make-interceptor 10 20)])
    ctx-summary)




(-> {:count 0}
    (enqueue [{:name :enqueue-more
               :enter
               (fn [ctx]
                 (enqueue ctx (repeat 10 (make-interceptor 1 0))))}])
    enter-1
    ctx-summary)




#_
(-> {:request {:uri "/hello"}}
    (enqueue [router])
    execute)



(defn terminate [ctx]
  (dissoc ctx :lambdaisland.interceptor/queue))


(-> {:count 0}
    (enqueue [{:name :enqueue-more
               :enter
               (fn [ctx]
                 (enqueue ctx (repeat 100
                                      {:name :add-1
                                       :enter
                                       (fn [ctx]
                                         (if (> (:count ctx) 20)
                                           (terminate ctx)
                                           (update ctx :count inc)))})))}])
    execute)


(def boom!
  {:name :BOOM!
   :enter (fn [ctx]
            (throw (ex-info "OOps!" {:very :sorry})))})

(def handle-error
  {:name :handle-error
   :error (fn [ctx ex]
            (update ctx :count * -1))})


(-> {:count 0}
    (enqueue [handle-error
              (make-interceptor 1 2)
              (make-interceptor 10 20)
              boom!
              (make-interceptor 100 200)])
    enter-1
    enter-1
    enter-1
    enter-1

    error-1
    error-1
    error-1
    error-1
    ctx-summary)
;;=>
{:count -11,
 :stack (),
 :error
 {:stage :enter,
  :interceptor :BOOM!,
  :exception-type :clojure.lang.ExceptionInfo,
  :very :sorry}}
