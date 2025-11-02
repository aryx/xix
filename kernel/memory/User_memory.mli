
module Operators : sig
val (@<): Types.user_addr -> Types.user_addr -> bool
val (@>): Types.user_addr -> Types.user_addr -> bool
val (@>=): Types.user_addr -> Types.user_addr -> bool

val (@+): Types.user_addr -> int -> Types.user_addr
val (@-): Types.user_addr -> Types.user_addr -> int
end

val roundup_page: Types.user_addr -> Types.user_addr
