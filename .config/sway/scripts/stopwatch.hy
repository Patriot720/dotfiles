#!/usr/bin/env hy
(import [dbus :as dbus])
(import [datetime [datetime]])

(setv bus (dbus.SystemBus))
(setv running False)
(setv counter 0)

(defn main-loop []
  (when running
      (global counter)
      (->
        (datetime.fromtimestamp counter)
        (.strftime  "%H:%M:%S")
        print)))

(defn stop []
  (global running)
  (setv running False))

(defn start []
  (global running)
  (setv running True))


(defmain []
  (bus.add-signal-receiver start "timer_start")
  (bus.add-signal-receiver stop "timer_stop")
  (while True
    (main-loop)
    (sleep 1)
    )
  )
