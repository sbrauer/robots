(ns robots.history)

(defn- undo
  [current-state {:keys [undos redos]}]
  (let [old-state (peek undos)]
    (if old-state
      {:state old-state :undos (pop undos) :redos (conj redos current-state)}
      {:state current-state :undos undos :redos redos})))

(defn- redo
  [current-state {:keys [undos redos]}]
  (let [old-state (peek redos)]
    (if old-state
      {:state old-state :redos (pop redos) :undos (conj undos current-state)}
      {:state current-state :undos undos :redos redos})))

(defn historize
  "Decorates the given function with undo/redo.
  f is a function that takes [state action]
  and returns a new state.
  Returns a function with the same arguments that intercepts
  the actions :undo and :redo and returns the corresponding state.
  Note that instead of returning the new state itself, the decorated
  function returns a hash with the keys :state, :undos and :redos"
  [f]
  (let [history (atom {:undos [] :redos []})]
    (fn [orig-state action]
      (let [{:keys [state undos redos]}
            (case action
              :undo (undo orig-state @history)
              :redo (redo orig-state @history)
              {:state (f orig-state action)
               :undos (conj (:undos @history) orig-state)
               :redos []})]
        (when (not= state orig-state)
          (swap! history assoc :undos undos :redos redos))
        (assoc @history :state state)))))
