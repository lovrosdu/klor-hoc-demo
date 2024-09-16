(ns klor-hoc-demo.core
  (:require
   [clojure.string :as str]
   [klor.core :refer :all]
   [klor.runtime :refer [play-role]]
   [klor.simulator :refer [simulate-chor]]
   [klor.sockets :refer [with-server with-accept with-client wrap-sockets]]))

;;; Starting Out
;;;
;;; - Defining a choreography (roles, choreographic type, params)
;;; - Concurrency model
;;; - Simulator
;;; - Communication basics

(defchor simple-1 [A B] (-> B) []
  (A (println "Hello!"))
  (B 123))

(defchor simple-2 [A B] (-> A B) [x]
  (A->B x))

(defchor remote-invoke [A B] (-> B A A) [f x]
  (B->A (f (A->B x))))

(defchor remote-apply [A B] (-> B A A) [f xs]
  (B->A (B (apply f (A->B xs)))))

;;; Sharing Knowledge
;;;
;;; - Agreement types
;;; - Conditionals and knowledge of choice

(defchor simple-3 [A B] (-> A #{A B}) [x]
  (A=>B x))

(defchor remote-map [A B] (-> B A A) [f xs]
  (if (A=>B (A (empty? xs)))
    (A nil)
    (A (cons (remote-invoke [A B] f (first xs))
             (remote-map [A B] f (next xs))))))

(defchor maybe-inc [A B] (-> A B) [x]
  (if (A=>B (A (= (rand-int 2) 0)))
    (B (inc (A->B x)))
    (B (println "Nothing!"))))

(defn read-creds [prompt]
  (print prompt)
  {:password (str/trim (read-line))})

(defchor auth [C A] (-> C #{C A}) [get-creds]
  (or (A=>C (A (= (:password (C->A (get-creds))) "secret")))
      (and (C=>A (C (rand-nth [true false])))
           (auth [C A] get-creds))))

(defchor get-token [C S A] (-> C C) [get-creds]
  (if (A=>S (auth [C A] get-creds))
    (S->C (S (random-uuid)))
    (C :error)))

;;; Returning Multiple Values
;;;
;;; - Choreographic tuples
;;; - Forced agreement

(defn modpow [base exp mod]
  (.modPow (biginteger base) (biginteger exp) (biginteger mod)))

(defchor exchange-key-1 [A B] (-> #{A B} #{A B} A B [A B]) [g p sa sb]
  (pack (A (modpow (B->A (B (modpow g sb p))) sa p))
        (B (modpow (A->B (A (modpow g sa p))) sb p))))

(defchor exchange-key-2 [A B] (-> #{A B} #{A B} A B #{A B}) [g p sa sb]
  (agree! (A (modpow (B->A (B (modpow g sb p))) sa p))
          (B (modpow (A->B (A (modpow g sa p))) sb p))))

;;; Higher-Order
;;;
;;; - Choreographies as values
;;; - Future work for type inference
;;; - Avoid auxiliary roles, defer to the tutorial/reference

(defchor chain [A B C] (-> (-> B C) (-> A B) A C) [g f x]
  (g (f x)))

(defchor chain-test [A B C] (-> C) []
  (chain [A B C]
         (chor (-> B C) [x] (B->C (B (+ x 10))))
         (chor (-> A B) [x] (A->B (A (* x 10))))
         (A 41)))

(defchor compose [A B C] (-> (-> B C) (-> A B) (-> A C | B)) [g f]
  (chor (-> A C) [x] (g (f x))))

;;; Escaping the Simulator
;;;
;;; - Tic-Tac-Toe over TCP

(def ttt-syms
  '[x o])

(def ttt-none
  '_)

(def ttt-lines
  (concat (for [i (range 3)] (for [j (range 3)] [i j]))
          (for [i (range 3)] (for [j (range 3)] [j i]))
          [(for [i (range 3)] [i i])]
          [(for [i (range 3)] [i (- 3 i 1)])]))

(defn ttt-board []
  (vec (repeat 3 (vec (repeat 3 ttt-none)))))

(defn ttt-place [board loc sym]
  (when (= (get-in board loc) ttt-none) (assoc-in board loc sym)))

(defn ttt-free [board]
  (for [i (range 3) j (range 3)
        :let [loc [i j]]
        :when (= (get-in board loc) ttt-none)]
    loc))

(defn ttt-winner-on [board locs]
  (let [syms (distinct (map #(get-in board %) locs))]
    (when (= (count syms) 1) (first syms))))

(defn ttt-winner [board]
  (or (some (set ttt-syms) (map #(ttt-winner-on board %) ttt-lines))
      (when (empty? (ttt-free board)) :draw)))

(defn ttt-fmt [board]
  (str/join "\n" (map #(str/join " " %) board)))

(defn ttt-index [board]
  (for [i (range 3)]
    (for [j (range 3)
          :let [loc [i j]
                sym (get-in board loc)]]
      (if (= sym ttt-none) (+ (* i 3) j 1) sym))))

(defn ttt-pick [board]
  (let [n (do (println (format "Pick a location [1-9]:"))
              (Long/parseLong (read-line)))
        loc [(quot (dec n) 3) (mod (dec n) 3)]]
    (if (not= (get-in board loc) ttt-none)
      (recur board)
      loc)))

(defchor ttt-play [A B] (-> #{A B} #{A B} #{A B}) [board idx]
  (A (println (str "\n" (ttt-fmt (ttt-index board)))))
  (if-let [winner (ttt-winner board)]
    winner
    (let [loc (A=>B (A (ttt-pick board)))
          board' (ttt-place board loc (get ttt-syms idx))]
      (if board'
        (ttt-play [B A] board' (- 1 idx))
        (ttt-play [A B] board idx)))))

(comment
  @(simulate-chor ttt-play (ttt-board) 0)

  (def ttt-server
    (future
      (with-server [ssc :port 7889]
        (loop []
          (println "Listening on" (str (.getLocalAddress ssc)))
          (with-accept [ssc sc]
            (println "Got client" (str (.getRemoteAddress sc)))
            (play-role (wrap-sockets {:role 'A} {'B sc} :log true)
                       ttt-play (ttt-board) 0))
          (recur)))))

  (with-client [sc :host "127.0.0.1" :port 7889]
    (println "Connected to" (str (.getRemoteAddress sc)))
    (play-role (wrap-sockets {:role 'B} {'A sc} :log true)
               ttt-play (ttt-board) 0))
  )
