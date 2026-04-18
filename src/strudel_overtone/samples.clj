(ns strudel-overtone.samples
  (:require [overtone.core :as ov]
            [taoensso.telemere :as tel]
            [clojure.string :as str]
            [strudel-overtone.pattern :as p]))

(defonce samples (atom {}))
(defonce sample-slices (atom {}))

(defn- clean-name [n]
  (keyword (str/replace (clojure.core/name (p/->name n)) #"^:" "")))

(defn load-sample!
  "Loads a sample into the registry.
   Name can be a keyword or string.
   Path is the filesystem path to the audio file."
  [name path]
  (if-let [buf (try (ov/load-sample path)
                    (catch Exception e
                      (tel/log!
                       :error
                       {:sample {:path path :failed (.getMessage e)}})
                      nil))]
    (let [k (clean-name name)]
      (swap! samples assoc k buf)
      (tel/log! :info {:sample-loaded {:name k :from-path path}})
      k)
    (do
      (tel/log! :warn {:sample-not-loaded {:name name :from-path path}})
      nil)))

(defn load-freesound!
  "Loads a Freesound sample into the registry by ID.
   Name can be a keyword or string.
   ID is the Freesound sample ID."
  [name id]
  (if-let [buf (try (ov/freesound id)
                    (catch Exception e
                      (tel/log!
                       :error
                       {:freesound {:id id :failed (.getMessage e)}})
                      nil))]
    (let [k (clean-name name)]
      (swap! samples assoc k buf)
      (tel/log! :info {:freesound-loaded {:name k :id id}})
      k)
    (do
      (tel/log! :warn {:freesound-not-loaded {:name name :id id}})
      nil)))

(defn slice-sample!
  "Creates a new virtual sample instrument from a slice of an existing sample.
   name: The name for the new instrument (e.g., :kick)
   source-name: The name of the loaded sample (e.g., :break)
   begin: Start position (0.0 to 1.0)
   end: End position (0.0 to 1.0)"
  [name source-name begin end]
  (let [s-name (clean-name name)
        src-name (clean-name source-name)]
    (swap! sample-slices assoc s-name {:source src-name :begin begin :end end})
    (tel/log! :info {:sample-sliced {:name s-name :from src-name}})))

(defn slice-sample-ms!
  "Like slice-sample!, but uses milliseconds instead of 0.0 to 1.0 fractions.
   Note: Currently only supports slicing base samples, not nested slices."
  [name source-name begin-ms end-ms]
  (let [s-name (clean-name name)
        src-name (clean-name source-name)
        buf (get @samples src-name)]
    (if buf
      (let [total-ms (* (:duration buf) 1000)
            begin (double (/ begin-ms total-ms))
            end (double (/ end-ms total-ms))]
        (slice-sample! s-name src-name begin end))
      (tel/log! :error {:slice-ms-failed {:name s-name :source src-name :reason "Source sample not found"}}))))

(defn sample-info
  "Returns information about a sample or slice by name."
  [name]
  (let [s-name (clean-name name)
        slice (get @sample-slices s-name)
        effective-name (if slice (:source slice) s-name)
        buf (get @samples effective-name)]
    (when buf
      (let [buf-info {:duration (:duration buf)
                      :n-channels (:n-channels buf)
                      :rate (:rate buf)
                      :path (:path buf)
                      :size (:size buf)}]
        (if slice
          (let [begin (:begin slice)
                end (:end slice)
                dur (* (:duration buf) (abs (- end begin)))]
            (merge buf-info
                   {:type :slice
                    :source effective-name
                    :begin begin
                    :end end
                    :duration dur
                    :full-duration (:duration buf)}))
          (assoc buf-info :type :sample))))))
