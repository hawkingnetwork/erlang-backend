

-define(SUPER(I), {I, {I, 'start_link', []}, 'permanent', 'infinity', 'supervisor', [I]}).

-define(WORKER(I), {I, {I, 'start_link', []}, 'permanent', 5000, 'worker', [I]}).

-define(TOPIC, topic).