-define('ZMQ_PAIR',         0).
-define('ZMQ_PUB',          1).
-define('ZMQ_SUB',          2).
-define('ZMQ_REQ',          3).
-define('ZMQ_REP',          4).
-define('ZMQ_XREQ',         5).
-define('ZMQ_XREP',         6).
-define('ZMQ_PULL',         7).
-define('ZMQ_PUSH',         8).
-define('ZMQ_XPUB',         9).
-define('ZMQ_XSUB',        10).

% ZMQ socket options.
-define('ZMQ_HWM',                1).
-define('ZMQ_SWAP',               3).
-define('ZMQ_AFFINITY',           4).
-define('ZMQ_IDENTITY',           5).
-define('ZMQ_SUBSCRIBE',          6).
-define('ZMQ_UNSUBSCRIBE',        7).
-define('ZMQ_RATE',               8).
-define('ZMQ_RECOVERY_IVL',       9).
-define('ZMQ_MCAST_LOOP',        10).
-define('ZMQ_SNDBUF',            11).
-define('ZMQ_RCVBUF',            12).
-define('ZMQ_RCVMORE',           13).
-define('ZMQ_FD',                14).
-define('ZMQ_EVENTS',            15).
-define('ZMQ_TYPE',              16).
-define('ZMQ_LINGER',            17).
-define('ZMQ_RECONNECT_IVL',     18).
-define('ZMQ_BACKLOG',           19).
-define('ZMQ_RECOVERY_IVL_MSEC', 20).
-define('ZMQ_RECONNECT_IVL_MAX', 21).

% ZMQ send/recv flags
-define('ZMQ_NOBLOCK',    1).
-define('ZMQ_SNDMORE',    2).

%% Types
-type ezmq_socket_type() :: pair | pub | sub | req | rep | xreq | xrep | pull | push | xpub | xsub.
-type ezmq_endpoint() :: string().
-type ezmq_error() :: {error, integer()}.
-type ezmq_data() :: iolist().

-opaque ezmq_context() :: binary().
-opaque ezmq_socket() :: binary().

-type ezmq_send_recv_flag() :: noblock | sndmore | {timeout, non_neg_integer()}.
-type ezmq_send_recv_flags() :: list(ezmq_send_recv_flag).

-type ezmq_sockopt() :: hwm | swap | affinity | identity | subscribe | unsubscribe | rate | recovery_ivl | mcast_loop |
                        sndbuf | rcvbuf | rcvmore | fd | events | linger | reconnect_ivl | backlog | recovery_ivl_msec |
                        reconnect_ivl_max.

-type ezmq_sockopt_value() :: integer() | iolist().
