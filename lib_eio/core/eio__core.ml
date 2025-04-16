module Promise = Promise
module Fiber = Fiber
module Switch = Switch
module Cancel = Cancel
module Exn = Exn
module Private = struct
  module Suspend = Suspend
  module Cells = Cells
  module Broadcast = Broadcast
  module Single_waiter = Single_waiter
  module Trace = Trace
  module Fiber_context = Cancel.Fiber_context
  module Debug = Debug

  module Effects = struct
    type 'a enqueue = 'a Suspend.enqueue
  end
end
