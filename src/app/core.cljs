(ns app.core
  "This namespace contains your application and is the entrypoint for 'yarn start'."
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            ["react" :as react]
            [clojure.string :as str]))

(defonce todos (r/atom (sorted-map)))

(defonce counter (r/atom 0))

(defn add-todo [text]
  (let [id (swap! counter inc)]
    (swap! todos assoc id {:id id :title text :done false})))

(defn toggle [id] (swap! todos update-in [id :done] not))
(defn save [id title] (swap! todos assoc-in [id :title] title))
(defn delete [id] (swap! todos dissoc id))

(defn mmap [m f a] (->> m (f a) (into (empty m))))
(defn complete-all [v] (swap! todos mmap map #(assoc-in % [1 :done] v)))
(defn clear-done [] (swap! todos mmap remove #(get-in % [1 :done])))

(defonce init (do
                (add-todo "Rename Cloact to Reagent")
                (add-todo "Add undo demo")
                (add-todo "Make all rendering async")
                (add-todo "Allow any arguments to component functions")
                (complete-all true)))

(defn todo-input [{:keys [title on-save on-stop input-ref]}]
  (let [val (r/atom title)]
    (fn [{:keys [id class placeholder]}]
      (let [stop (fn [_e]
                   (reset! val "")
                   (when on-stop (on-stop)))
            save (fn [e]
                   (let [v (-> @val str str/trim)]
                     (when-not (empty? v)
                       (on-save v))
                     (stop e)))]
        [:input {:type "text"
                 :value @val
                 :ref input-ref
                 :id id
                 :class class
                 :placeholder placeholder
                 :on-blur save
                 :on-change (fn [e]
                              (reset! val (-> e .-target .-value)))
                 :on-key-down (fn [e]
                                (case (.-which e)
                                  13 (save e)
                                  27 (stop e)
                                  nil))}]))))

(defn todo-edit [props]
  (let [ref (react/useRef)]
    (react/useEffect (fn []
                       (.focus (.-current ref))
                       js/undefined))
    [todo-input (assoc props :input-ref ref)]))

(defn todo-stats [{:keys [filt active done]}]
  (let [props-for (fn [name]
                    {:class (when (= name @filt) "selected")
                     :on-click #(reset! filt name)})]
    [:div
     [:span#todo-count
      [:strong active] " " (case active 1 "item" "items") " left"]
     [:ul#filters
      [:li [:a (props-for :all) "All"]]
      [:li [:a (props-for :active) "Active"]]
      [:li [:a (props-for :done) "Completed"]]]
     (when (pos? done)
       [:button#clear-completed {:on-click clear-done}
        "Clear completed " done])]))

(defn todo-item []
  (let [editing (r/atom false)]
    (fn [{:keys [id done title]}]
      [:li
       {:class [(when done "completed ")
                (when @editing "editing")]}
       [:div.view
        [:input.toggle
         {:type "checkbox"
          :checked done
          :on-change #(toggle id)}]
        [:label
         {:on-double-click #(reset! editing true)}
         title]
        [:button.destroy {:on-click #(delete id)}]]
       (when @editing
         [:f> todo-edit {:class "edit"
                         :title title
                         :on-save #(save id %)
                         :on-stop #(reset! editing false)}])])))

(defn todo-app []
  (let [filt (r/atom :all)]
    (fn []
      (let [items (vals @todos)
            done (->> items (filter :done) count)
            active (- (count items) done)]
        [:div
         [:section#todoapp
          [:header#header
           [:h1 "todos"]
           [todo-input {:id "new-todo"
                        :placeholder "What needs to be done?"
                        :on-save add-todo}]]
          (when (-> items count pos?)
            [:div
             [:section#main
              [:input#toggle-all {:type "checkbox" :checked (zero? active)
                                  :on-change #(complete-all (pos? active))}]
              [:label {:for "toggle-all"} "Mark all as complete"]
              [:ul#todo-list
               (for [todo (filter (case @filt
                                    :active (complement :done)
                                    :done :done
                                    :all identity) items)]
                 ^{:key (:id todo)} [todo-item todo])]]
             [:footer#footer
              [todo-stats {:active active :done done :filt filt}]]])]
         [:footer#info
          [:p "Double-click to edit a todo"]]]))))

(defn articles [items]
  (if-not (seq items)
    [:div.article-preview "Loading"]
    (if (= 0 (count items))
      [:div.article-preview "No articles are here."]
      [:div
       (for [article items]
         [:h2 (:title article)])])))

(defn header-component []
  [:header.bg-gray-50
   [:div.mx-auto.max-w-screen-xl.px-4.py-8.sm:px-6.lg:px-8
    [:div.flex.items-center.sm:justify-between.sm:gap-4
     [:div.relative.hidden.sm:block
      [:label.sr-only {:for "search"} "Search"]
      [:input.h-10.w-full.rounded-lg.border-none.bg-white.pe-10.ps-4.text-sm.shadow-sm.sm:w-56
       {:id "search"
        :type "search"
        :placeholder "Search website..."}]
      [:button {:class "absolute.end-1.top-1/2.-translate-y-1/2.rounded-md.bg-gray-50.p-2.text-gray-600.transition.hover:text-gray-700" :type "button"}
       [:span.sr-only "Search"]
       [:svg
        {:xmlns "http://www.w3.org/2000/svg"
         :class "h-4 w-4"
         :fill "none"
         :viewBox "0 0 24 24"
         :stroke "currentColor"
         :stroke-width "2"}
        [:path
         {:stroke-linecap "round"
          :stroke-linejoin "round"
          :d "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"}]]]]
     [:div.flex.flex-1.items-center.justify-between.gap-8.sm:justify-end
      [:div.flex.gap-4
       [:button.block.shrink-0.rounded-lg.bg-white.p-2.5.text-gray-600.shadow-sm.hover:text-gray-700.sm:hidden
        {:type "button"}
        [:span.sr-only "Search"]
        [:svg
         {:xmlns "http://www.w3.org/2000/svg"
          :class "h-5 w-5"
          :fill "none"
          :viewBox "0 0 24 24"
          :stroke "currentColor"
          :stroke-width "2"}
         [:path
          {:stroke-linecap "round"
           :stroke-linejoin "round"
           :d "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"}]]]
       [:a.block.shrink-0.rounded-lg.bg-white.p-2.5.text-gray-600.shadow-sm.hover:text-gray-700
        {:href "#"}
        [:span.sr-only "Academy"]
        [:svg
         {:xmlns "http://www.w3.org/2000/svg"
          :class "h-5 w-5"
          :fill "none"
          :viewBox "0 0 24 24"
          :stroke "currentColor"
          :stroke-width "2"}
         [:path
          {:d "M12 14l9-5-9-5-9 5 9 5z"}]
         [:path
          {:d "M12 14l6.16-3.422a12.083 12.083 0 01.665 6.479A11.952 11.952 0 0012 20.055a11.952 11.952 0 00-6.824-2.998 12.078 12.078 0 01.665-6.479L12 14z"}]
         [:path
          {:stroke-linecap "round"
           :stroke-linejoin "round"
           :d "M12 14l9-5-9-5-9 5 9 5zm0 0l6.16-3.422a12.083 12.083 0 01.665 6.479A11.952 11.952 0 0012 20.055a11.952 11.952 0 00-6.824-2.998 12.078 12.078 0 01.665-6.479L12 14zm-4 6v-7.5l4-2.222"}]]]
       [:a.block.shrink-0.rounded-lg.bg-white.p-2.5.text-gray-600.shadow-sm.hover:text-gray-700
        {:href "#"}
        [:span.sr-only "Notifications"]
        [:svg
         {:xmlns "http://www.w3.org/2000/svg"
          :class "h-5 w-5"
          :fill "none"
          :viewBox "0 0 24 24"
          :stroke "currentColor"
          :stroke-width "2"}
         [:path
          {:stroke-linecap "round"
           :stroke-linejoin "round"
           :d "M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"
           }]]]]
      [:button {:type "button" :class "group flex shrink-0 items-center rounded-lg transition"}
       [:span.sr-only "Menu"]
       [:img
        {:alt "Man"
         :src "https://images.unsplash.com/photo-1600486913747-55e5470d6f40?ixlib=rb-1.2.1&ixid=MnwxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8&auto=format&fit=crop&w=1770&q=80"
         :class "h-10 w-10 rounded-full object-cover"}]
       [:p.ms-2.hidden.text-left.text-xs.sm:block
        [:strong.block.font-medium "Eric Frusciante"]
        [:span.text-gray-500 " eric@frusciante.com"]]
       [:svg
        {:xmlns "http://www.w3.org/2000/svg"
         :class "ms-4 hidden h-5 w-5 text-gray-500 transition group-hover:text-gray-700 sm:block"
         :viewBox "0 0 20 20"
         :fill "currentColor"}
        [:path
         {:fill-rule "evenodd"
          :d "M5.293 7.293a1 1 0 011.414 0L10 10.586l3.293-3.293a1 1 0 111.414 1.414l-4 4a1 1 0 01-1.414 0l-4-4a1 1 0 010-1.414z"
          :clip-rule "evenodd"}]]]]]
   [:div.mt-8
    [:h1.text-2xl.font-bold.text-gray-900.sm:text-3xl "Welcome Back, Barry!"]
    [:p.mt-1.5.text-sm.text-gray-500
     "Your website has seen a 52% increase in traffic in the last month. Keep it up! 🚀"]]]])


(defn navbar []
  [:nav.navbar.navbar-expand-lg.navbar-dark.bg-dark
    [:div.container-fluid
      [:a.navbar-brand ^{:class ["bg-blue-100"
              "hover:bg-blue-600"
              "text-white"
              "font-bold"
              "py-2"
              "px-4"
              "rounded"
              "text-600"]} {:href "#"} "Navbar"]
      [:button.navbar-toggler
       {:type "button"
        :data-bs-toggle "collapse"
        :data-bs-target "#navbarNav"
        :aria-controls "navbarNav"
        :aria-expanded "false"
        :aria-label "Toggle navigation"}
       [:span.navbar-toggler-icon]]
      [:div.collapse.navbar-collapse {:id "navbarNav"}
        [:ul.navbar-nav
          [:li.nav-item
           [:a.nav-link.active {:aria-current "page" :href "#"} "Home"]]
          [:li.nav-item
           [:a.nav-link {:href "#"} "Features"]]
          [:li.nav-item
           [:a.nav-link {:href "#"} "Pricing"]]
          [:li.nav-item
           [:a.nav-link.disabled {:aria-disabled "true"} "Disabled"]]]]]])
 

(defn main-view []
  [:div.col-md-9
   [:div.feed-toggle
    [:ul.nav.nav-pills.outline-active
     [:li.nav-item
      [:a.nav-link.active {:href ""} "Global Feed"]]]]
   [articles]])

(defn header []
  [:nav.navbar.bg-dark
    [:div.container
      [:a.navbar-brand.text-success {:href ""}"Conduit"]]])

(defn banner [token]
  (when token
    [:div.banner>div.container
      [:h1.logo-front "conduit"]
        [:p "A place to share your knowledge."]])
  [:div.banner>div.container "Banner"])

(defn home-page[]
  [:div.home-page
   [banner "auth-user-token"]
   [:div.container.page>div.row
    [main-view]
    [:div.col-md-3
     [:div.sidebar
      [:p "Popular tags"]]]]])

(defn app []
  [:div 
    [todo-app]])

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (r/render [app] (.getElementById js/document "app")))

(defn ^:export main
  "Run application startup logic."
  []
  (render))

(enable-console-print!)
(js/require "bootstrap/dist/css/bootstrap.min.css")

(js/console.log "Hello from cljs world")
