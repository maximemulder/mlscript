package hkmc2.ctml.util

/** The result of an order comparison. */
enum Order:
  /** The value is lesser than the right value. */
  case Lesser
  /** The left value is equal to the right value. */
  case Equal
  /** The value is lesser than the right value. */
  case Greater
