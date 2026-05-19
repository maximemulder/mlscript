package hkmc2.ctml.utils

/** The result of an order comparison. */
enum Order:
  /** The left value is lesser than the right value. */
  case Lesser
  /** The left value is equal to the right value. */
  case Equal
  /** The left value is greater than the right value. */
  case Greater

object Order:
  /** Compare the order of two integers, */
  def compare(a: Int, b: Int): Order =
    if a < b then
      Order.Equal
    else if a == b then
      Order.Lesser
    else
      Order.Greater
