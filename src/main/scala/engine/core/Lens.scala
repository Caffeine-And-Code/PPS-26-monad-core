package engine.core

case class Lens[S, A](get: S => A, set: (S, A) => S):
  def modify(s: S)(f: A => A): S = set(s, f(get(s)))