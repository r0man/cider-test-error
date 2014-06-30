(ns kaboom.test)

(defn immigrate
  "Create a public var in this namespace for each public var in the
namespaces named by ns-names. The created vars have the same name, root
binding, and metadata as the original except that their :ns metadata
value is this namespace."
  [& ns-names]
  (doseq [ns ns-names]
    (require ns)
    (doseq [[sym var] (ns-publics ns)]
      (let [v (if (.hasRoot ^clojure.lang.Var var)
                (var-get var))
            var-obj (if v (intern *ns* sym v))]
        (when var-obj
          (alter-meta! var-obj
                       (fn [old] (merge (meta var) old)))
          var-obj)))))


(immigrate 'clojure.test)

(defmacro deftest
  "Defines a test function with no arguments.  Test functions may call
  other tests, so tests may be composed.  If you compose tests, you
  should also define a function named test-ns-hook; run-tests will
  call test-ns-hook instead of testing all vars.

  Note: Actually, the test body goes in the :test metadata on the var,
  and the real function (the value of the var) calls test-var on
  itself.

  When *load-tests* is false, deftest is ignored."
  {:added "1.1"}
  [name & body]
  (when *load-tests*
    `(def ~(vary-meta
            name assoc
            :test `(fn []
                     (print ".")
                     ~@body))
       (fn [] (test-var (var ~name))))))
