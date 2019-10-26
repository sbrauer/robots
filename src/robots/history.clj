(ns robots.history)

(defn- undo
  [{:keys [state undos redos] :as context}]
  (if (seq undos)
    {:state (peek undos) :undos (pop undos) :redos (conj redos state)}
    context))

(defn- redo
  [{:keys [state undos redos] :as context}]
  (if (seq redos)
    {:state (peek redos) :redos (pop redos) :undos (conj undos state)}
    context))

(defn historize
  "Decorates the given function with undo/redo support.

  f is assumed to be a 2-arity function that takes [state action]
  and returns a new state.

  Returns a function that:
  - wraps f such that it intercepts the actions :undo and :redo
  - instead of taking state and action takes a history context map with the keys [:state :undos :redos] and an action
  - returns a new history context map"
  [f]
  (fn [{:keys [state undos redos] :as context} action]
    (case action
      :undo (undo context)
      :redo (redo context)
      {:state (f state action)
       :undos (conj undos state)
       :redos nil})))
