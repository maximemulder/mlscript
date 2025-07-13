package hkmc2.ctml.util

/** The result of an order comparison. */
enum Order:
  /** The left value is lesser than the right value. */
  case Lesser
  /** The left value is equal to the right value. */
  case Equal
  /** The left value is greater than the right value. */
  case Greater
