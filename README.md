# octobind

An elementary multibind facility for com.stuartsierra.component.

## Usage

Occasionally it is useful to be able to acquire a collection of
dependencies rather than individual dependencies. 

````clojure

(defrecord Contribution [listeners])
(def mod1 (Contribution. [notify-updates send-log])
(def mod2 (Contribution. [track-logins])
(def mod3 (Contribution. [roll-buffer])

(defrecord Eventer [contribs listeners]
  component/Lifecycle
  (start [self]
    (assoc self :listeners (mapcat :listeners (contribs))))
  (stop [self] self))

(component/system-map
  :eventer (component/using (eventer) [:contribs])
  :contribs (multi/bind mod1 mod2 mod3)

````

## License

Copyright © 2015 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
