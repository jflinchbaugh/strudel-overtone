(ns strudel-overtone.util)

(defmacro import-vars
  "Imports multiple vars from a namespace or alias, preserving metadata (docstrings, arglists)."
  [ns-alias-or-sym & syms]
  (let [ns-obj (or (find-ns ns-alias-or-sym)
                   (get (ns-aliases *ns*) ns-alias-or-sym))]
    `(do
       ~@(for [s syms]
           (let [v (ns-resolve ns-obj s)
                 m (meta v)
                 clean-m (select-keys m [:doc :arglists])
                 orig-sym (symbol (str (ns-name ns-obj)) (str s))]
             `(do
                (def ~s ~orig-sym)
                (alter-meta! (var ~s) merge '~clean-m)))))))
