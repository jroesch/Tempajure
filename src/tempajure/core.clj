(ns tempajure.core
  (:require (clojure.java.io reader)))
  
  (defn fst [x] (x 0))
  
  (defn snd [x] (x 1))
  
  (def open-brace "{{")
  
  (def close-brace "}}")
  
  (defn read-contents [r]
    (loop [contents ""]
      (let [line (.readLine r)]
        (if (= line nil)
            contents
          (recur (str contents line "\n"))))))
          
  (defn read-file [f]
    (read-contents (reader f)))
  
  (def grtr-zero (every-pred #(< 0 %)))
  
  (defn valid-idx? [is]
    (apply grtr-zero is))
  
  (defn indices [s]
    (let [start (.indexOf s "{{")
          close (.indexOf s "}}")]
      [start close]))
             
  (defn split [s is]
    [(.substring s 0 (fst is)) 
     { :template (.substring s (+ 2 (fst is)) (snd is)) }
     (.substring s (+ 2 (snd is)))])
  
  ; Non-efficent recursion 
  (defn break-up [s]
    (let [indxs (indices s)]
      (if (valid-idx? indxs)
          (let [all (split s indxs)
                f   (all 0)
                m   (all 1)
                r   (all 2)]
            (cons f (cons m (break-up r))))
        (list s))))
  ; broken if symbols are pulled in
  (defmacro expand-temp [env code]
    (let [map-to-bind (fn [v] [(symbol (name (first v))) (second v)])
          bindings (flatten (map map-to-bind (seq `env)))
          read-code (read-string `code)]
      `(let [~@bindings]
         ~read-code))) 
         
  ; (defn fill-section [s env]
  ;   (if (map? s)
  ;       (let [code (s :template)]
  ;         (expand-temp env code))
  ;     s))
            
  (defn fill-template [t env]
    (reduce "" #(fn [x y] (str x "\n" y)) 
      (map #(fill-section % env) t)))
        
  (defn print-each [x]
    (map println x))

           
      
      
  
      
    
          
  