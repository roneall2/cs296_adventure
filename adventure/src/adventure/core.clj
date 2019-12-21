(ns adventure.core
  (:gen-class))
(require '[clojure.string :as str])

(def init-map
  {:foyer {:desc "You're standing at your front door and you're pretty hungry. You should look for something to eat. Ahead of you is a hallway to the north. "
            :title "in the foyer"
            :dir {:south :lawn :north :hallway}
            :contents #{}}
    :lawn {:desc "You're on your lawn. You thought it would be neat if you grew tomatoes and carrots out here. The front door is to the north. "
              :title "outside"
              :dir {:north :foyer}
              :contents #{:tomato :carrot}}
    :hallway {:desc "The foyer is behind you to the south, the kitchen is to the east, the stairs are to the west, and the backroom is north. "
              :title "in the first floor hallway"
              :dir {:south :foyer :east :kitchen :west :stairs :north :backroom}
              :contents #{}}
    :kitchen {:desc "Where the magic happens! You can only cook in this room. You think you left a few ingredients lying around here. The hallway is west. "
              :title "in the kitchen"
              :dir {:west :hallway}
              :contents #{:bread :noodles}}
    :stairs {:desc "A bridge between lands, the staircase connects the three floors of the house. Hallway to the east, upstairs is north, basement is south. "
             :title "at the staircase"
             :dir {:east :hallway :north :bedroom :south :basement}
             :contents #{}}
    :backroom {:desc "A weird room at the back of your house. Smells a bit...cheesy. There's a hallway to the south. "
               :title "in the backroom"
               :dir {:south :hallway}
               :contents #{:cheese}}
    :bedroom {:desc "Looking at your bed makes you sleepy. You notice you left a can of soup broth on your nightstand for some reason. The stairway is south. "
              :title "in your bedroom"
              :dir {:south :stairs}
              :contents #{:broth}}
    :basement {:desc "It's cold in your basement. You left some meat here, since this room acts as a sort of natural freezer. The stairway is north. "
               :title "in the basement"
               :dir {:north :stairs}
               :contents #{:ham :turkey}}
    })

(def init-items
  {:tomato {:desc "This is a fresh tomato! It's a nice, juicy snack, but is it a fruit or a vegetable?"
              :name "a tomato"
              :score 2
              :cook :blt}
    :carrot {:desc "A firm, young carrot. Nice and crunchy, good for your eyesight."
             :name "a carrot"
             :score 2
             :cook :carrot-cake}
    :bread {:desc "Some white bread. Could probably be cooked to make toast."
            :name "bread"
            :score 2
            :cook :toast}
    :noodles {:desc "Raw noodles that could be used to make some nice pasta. They probably don't taste good raw."
              :name "a package of noodles"
              :score 0
              :cook :spaghetti}
    :cheese {:desc "Cheese! Cheese for everyone! Oh, I'm sorry, what were you saying?"
             :name "CHEESE!"
             :score 3
             :cook :quesadilla}
    :broth {:desc "Uncooked soup broth. Probably inedible without being cooked..."
            :name "some soup broth"
            :score 0
            :cook :soup}
    :ham {:desc "Some good ham! Great on its own, better cooked!"
          :name "some ham"
          :score 3
          :cook :glazed-ham}
    :turkey {:desc "A fair amount of deli turkey. Yum!"
             :name "some turkey"
             :score 2
             :cook :thanksgiving-turkey}

    :blt {:desc "A delicious BLT sandwich! Makes you feel healthy, even if there is bacon on it..."
          :name "a BLT"
          :score 5
          :cook nil}
    :carrot-cake {:desc "Carrot cake: a bunny's favorite dessert."
                  :name "some carrot cake"
                  :score 4
                  :cook nil}
    :toast {:desc "Lightly buttered yet slightly burnt, this is basically your average piece of toast."
            :name "a piece of toast"
            :score 3
            :cook nil}
    :spaghetti {:desc "A wonderfully delicious plate of homemade spaghetti. Mamma mia!"
                :name "a plate of spaghetti"
                :score 7
                :cook nil}
    :quesadilla {:desc "A quesadilla you whipped up in the pan, served with salsa and guac on the side."
                 :name "a quesadilla"
                 :score 6
                 :cook nil}
    :soup {:desc "A nice bowl of chicken noodle soup to warm you up. :)"
           :name "a bowl of soup"
           :score 6
           :cook nil}
    :glazed-ham {:desc "Holy moley! This ham has been cooked to perfection and glazed with brown sugar - it tastes amazing!"
                 :name "glazed ham"
                 :score 10
                 :cook nil}
    :thanksgiving-turkey {:desc "Turkey prepared Thanksgiving style - stuffing and all. Dig in!"
                          :name "Thanksgiving turkey"
                          :score 8
                          :cook nil}
  })

(def init-adventurer
  {:location :foyer
    :inventory #{}
    :score 0
    :num-eaten 0
    :tick 0
    :seen #{}})

;; Here is a routine that prints out the status every time the adventurer enters a room.
(defn status [state]
  (let [location (get-in state [:adventurer :location])
        the-map (:map state)]
    (print (str "You are " (-> the-map location :title) ". "))
    (when-not (contains? (get-in state [:adventurer :seen]) location)
      (print (-> the-map location :desc)))
    (update-in state [:adventurer :seen] #(conj % location))))

;; Here is a routine to go somewhere.
(defn go [state dir]
  (let [location (get-in state [:adventurer :location])
        dest ((get-in state [:map location :dir]) dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          state)
      (update-in (assoc-in state [:adventurer :location] dest) [:adventurer :tick] inc))))

;; Let's pick up an item!
(defn pickup [state item]
  (let [location (get-in state [:adventurer :location])]
  (loop [contents (get-in state [:map location :contents])]
    (if (empty? contents)
      (do (println "That item doesn't exist!")
          state)
      (if (= (first contents) item)
        (update-in (update-in state [:adventurer :inventory] conj item) [:map location :contents] disj item)
        (recur (rest contents)))))))

;; Let's drop an item!
(defn drop-item [state item]
  (let [location (get-in state [:adventurer :location])]
    (loop [contents (get-in state [:adventurer :inventory])]
      (cond
        (empty? contents)
          (do (println "You don't have that!")
              state)
        :else
          (if (= (first contents) item)
            (update-in (update-in state [:map location :contents] conj item) [:adventurer :inventory] disj item)
            (recur (rest contents)))))))

;; Let's check our inventory!
(defn inventory [state]
  (let [inv (get-in state [:adventurer :inventory])]
    (if (empty? inv)
      (do (println "Your inventory is empty!") state)
      (do (println "INVENTORY:")
        (doseq [item inv]
          (println (get-in state [:items item :name])))
          state)
    )))

;; Let's get outta here.
(defn quit [state]
  (do 
    (println "Game Over! Your score:" (get-in state [:adventurer :score]))
    (System/exit 0)))

;; Function to look around the room.
(defn look [state]
  (let [location (get-in state [:adventurer :location])]
    (do (println (get-in state [:map location :desc]))
      (doseq [item (get-in state [:map location :contents])] 
        (print "There is" (get-in state [:items item :name]) ". "))
      state)))

;; Let's examine an item!
(defn examine [state item]
  (if-let [desc (get-in state [:items item :desc])]
    (do (println desc) state)
    (do (println "I don't know what that is.") state)))

;; Time to eat!
(defn eat [state item]
  (if-let [food (get-in state [:adventurer :inventory item])]
    (do (println "Yum!")
      (update-in 
        (update-in 
          (update-in state [:adventurer :score] + (get-in state [:items food :score]))
        [:adventurer :inventory] disj food)
      [:adventurer :num-eaten] + 1)
    )
    (do (println "You can't eat what you don't have...") state)
  )
)

;; help function
(defn help [state]
  (do (println "")
    (println "LIST OF COMMANDS: ")
    (println "GO: Type 'go [direction]' or just '[direction]' to move to that room.")
    (println "TAKE: Type 'take [item]' to add it to your inventory.")
    (println "DROP: Type 'drop [item]' to remove it from your inventory and drop it in the current room.")
    (println "INVENTORY: Type 'inventory' or just 'i' to look at your inventory.")
    (println "LOOK: Type 'look' to look around the current room. Will display available items.")
    (println "EXAMINE: Type 'examine [item]' to learn more about it.")
    (println "EAT: Type 'eat [item]' to eat it! You need to eat 4 food items to win.")
    (println "COOK: Type 'cook [item]' to make a dish with it, if you're in the kitchen. Cooking it may increase its point total.")
    (println "QUIT: Type 'quit' to end the game early.")
    (println "HELP: Type 'help' or 'h' to see this again!")
    (println "")
    (println "A note about syntax: when referring to an item, use a simple one word version if you can (i.e. 'a can of broth' should just be 'broth').")
    (println "If an item cannot be easily referred to in one word, two words separated by a dash will work (i.e. 'carrot-cake').")
    (println "Finally, did you know that each food item has a different point total? The maximum number of points you can earn in one game is 31.")
    (println "")
    state))

;; cooking time
(defn cook [state item]
  (if (= (get-in state [:adventurer :location]) :kitchen)
    (if-let [food (get-in state [:adventurer :inventory item])]
      (let [new-food (get-in state [:items food :cook])]
        (if (nil? new-food)
          (do (println "That food's already been cooked!") state)
          (do (println "This looks tasty...")
            (update-in (update-in state [:adventurer :inventory] conj new-food) [:adventurer :inventory] disj food)
          )
        )
      )
      (do (println "You can't cook what you don't have...") state)
    )
    (do (println "You can't cook if you're not in the kitchen!") state)
  )
)

;; # Action Environment
;;
;; The runtime environment is a vector of the form
;;
;; ``` [ phrase action phrase action ...]```
;;
;; The "phrase" is a canonicalized vector of keywords.
;; (Remember that a keyword is a symbol starting with a colon, like :name.)
;; The `@` character will represent a variable.

(def initial-env [  
  [:go "@"] go
  [:take "@"] pickup
  [:drop "@"] drop-item
  [:i] inventory
  [:inventory] inventory
  [:quit] quit
  [:look] look
  [:examine "@"] examine
  [:eat "@"] eat
  [:help] help
  [:h] help
  [:cook "@"] cook
  ["@"] go
  ])  ;; add your other functions here

;; Parsing code 
(defn c0da
  [keys strs]
  (if (empty? strs)
    keys
    (c0da (conj keys (keyword (first strs))) (rest strs)))
)

(defn canonicalize
  "Given an input string, strip out whitespaces, lowercase all words, and convert to a vector of keywords."
  [input]
    (c0da [] (str/split (str/lower-case input) #" "))
)

;; respond code
(defn match [pattern input]
  (loop [pattern pattern  
         input input
         vars '()]
    (cond (and (empty? pattern) (empty? input)) (reverse vars)
      (or (empty? pattern) (empty? input)) nil
      (= (first pattern) "@")
        (recur (rest pattern)
          (rest input)
          (cons (first input) vars))
      (= (first pattern) (first input))
        (recur (rest pattern)
          (rest input)
          vars)
      :fine-be-that-way nil
)))

(defn respond
  "Given a state and a canonicalized input vector, search for a matching phrase and call its corresponding action.
  If there is no match, return the original state and result \"I don't know what you mean.\""
  [state input-vector]
  (if (= (get-in state [:adventurer :num-eaten]) 4)

    (do (println "You've had enough food and feel full now. You win!")
      (quit state))

    (loop [patterns initial-env]
      (if (empty? patterns)
        (do (println "I don't know what you mean.")
          state)
      (if-let [args (match (first patterns) input-vector)]
        (cond
          (= (count args) 0)
            ((nth patterns 1) state) 
          (= (count args) 1)
            ((nth patterns 1) state (first args))
          (= (count args) 2)
            ((nth patterns 1) state (first args) (first (next args)))
          :size-three 
            ((nth patterns 1) state (first args) (first (next args)) (first (next (next args))))
        )
        (recur (rest (rest patterns)))
      ))
    )
  )
)

;; Here is a main function to start the game.
(defn -main
  "Initialize the adventure"
  [& args]
  (loop [local-state {:map init-map :adventurer init-adventurer :items init-items}]
    (let [pl (status local-state) 
          _  (println "What do you want to do?")
          command (read-line)]
      (recur (respond pl (canonicalize command))))))
