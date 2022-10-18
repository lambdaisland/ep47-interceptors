(ns lambdaisland.scratch)

(:require [io.pedestal.interceptor :as interceptor :refer [interceptor]]
          [io.pedestal.interceptor.chain :as chain]
          [clojure.core.async :as async])

(-> {:count 0}
    (enqueue [{:enter (fn [ctx]
                        (update ctx :count + 1))}
              {:enter (fn [ctx]
                        (update ctx :count + 10))}])
    execute)
;; =>
;; {:count 11,
;;  :lambdaisland.interceptor/queue <-()-<,
;;  :lambdaisland.interceptor/stack ()}


(-> {:count 0}
    (enqueue [{:name  :add-1
               :enter (fn [ctx]
                        (update ctx :count + 1))}
              {:name  :add-2
               :enter (fn [ctx]
                        (update ctx :count + 10))}]))
;; =>
;; {:count 0,
;;  :lambdaisland.interceptor/queue
;;  <-({:name :add-1,
;;      :enter #function[lambdaisland.interceptor-demo/eval13368/fn--13369]}
;;     {:name :add-2,
;;      :enter #function[lambdaisland.interceptor-demo/eval13368/fn--13371]})-<}

(-> {:count 0}
    (enqueue [{:name  :add-1
               :enter (fn [ctx]
                        (update ctx :count + 1))}
              {:name  :add-2
               :enter (fn [ctx]
                        (update ctx :count + 10))}])
    :lambdaisland.interceptor/queue
    type)
;;=> clojure.lang.PersistentQueue

(-> clojure.lang.PersistentQueue/EMPTY
    (conj :a)
    (conj :b)
    (conj :c)
    (pop))
;;=> <-(:b :c)-<

(defn add-interceptor [x y]
  {:name  (keyword (str "add-" x "-" y))
   :enter (fn [ctx]
            (update ctx :count + x))
   :leave  (fn [ctx]
             (update ctx :count + y))})

(-> {:count 0}
    (enqueue [(add-interceptor 1 2)
              (add-interceptor 10 20)])
    enter-1
    enter-1
    leave-1
    ctx-summary)
;;=> {:count 31, :queue <-()-<, :stack (:add-1-2)}


(-> {:count 0}
    (enqueue [{:name :enqueue-more
               :enter
               (fn [ctx]
                 (enqueue ctx (repeat 10 (add-interceptor 1 0))))}
              ])
    enter-1
    ctx-summary)

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
            (throw (ex-info "Oops!" {:very :sorry})))})

(def handle-error
  {:name :handle-error
   :error (fn [ctx ex]
            (update ctx :count * -1))})

(-> {:count 0}
    (enqueue [handle-error
              (add-interceptor 1 2)
              (add-interceptor 10 20)
              boom!
              (add-interceptor 100 200)])
    process-enter
    process-error
    ctx-summary)

(-> {:count 0}
    (chain/execute [(interceptor/interceptor (add-interceptor 1 2))
                    (interceptor/interceptor (add-interceptor 10 20))]))

(require '[clojure.core.async :as async])

(defn get-user-info [{:keys [user pwd]}]
  (let [ch (async/chan 1)]
    (async/>!! ch {:username "Arne"})
    ch))

(defn get-user-info [request]
  (async/go
    {:username "Arne"}))

(defn handle-login [{:keys [request] :as ctx}]
  (async/go
    (let [{:keys [username]} (async/<! (get-user-info request))]
      (assoc ctx
             :response
             {:body (if username
                      (str "welcome back " username)
                      "sorry, try again")}))))

(-> {:request {:uri "/login", :request-method :post, :params {:user "xxx", :pwd "sekrit"}}}
    (chain/execute [(interceptor/interceptor {:enter handle-login})]))

(-> {:request {:uri "/login", :request-method :post, :params {:user "xxx", :pwd "sekrit"}}}
    (chain/execute [(interceptor/interceptor
                     {:leave (fn [ctx]
                               (prn :response '=> (:response ctx)))})
                    (interceptor/interceptor {:enter handle-login})]))

(-> {:request {:uri "/", :request-method :get, :headers {}}}
    (chain/execute [(interceptor/interceptor
                     {:enter
                      (fn [ctx]
                        (assoc ctx :response {:status 200
                                              :body "hello, world"
                                              :headers {"Content-Type" "text/plain"}}))})]))


(def ^:dynamic *foo*)

(chain/execute {}
               [(interceptor {:enter (fn [ctx] (assoc ctx :bindings {#'*foo* 123}))})
                (interceptor {:enter (fn [ctx] {:response {:body (str "*foo* = " *foo*)}})})])

(-> {:count 0}
    (chain/terminate-when (fn [{:keys [count]}] (< count 0)))
    (chain/execute [(interceptor {:enter (fn [ctx] (update ctx :count dec))})
                    (interceptor {:enter (fn [ctx] (update ctx :count + 10))})]))
