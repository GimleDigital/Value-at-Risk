package valueatrisk.chapter03

import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants._

/** Help functions for statistics and distributions */
object statistics {
  /** Represents the normal distribution for a mean and a variance
   *
   *  @param mean the mean of the distribution
   *  @param variance the variance of the distribution
   */
  case class NormalDistribution (mu: Double, variance: Double) {
    val sigma = sqrt(variance)

    /** Returns the value of the distribution's inverse cumulative distribution
     *  function for a given probability.
     *
     *  Adapted from breeze.stats.distributions.Gaussian (C) which is
     *  Copyright 2009 David Hall, Daniel Ramage under Apache 2.0 license
     *
     *  @param p the probability
     */
    def inverseCdf = (p: Double) => mu + sigma * sqrt(2.0) * erfinv(2 * p - 1)

    /** Returns the first order differential of the distribution's inverse
     *  cumulative distribution function for a given probability.
     *
     *  @param p the probability
     */
    def inverseCdfDiff = (p: Double) => 2 * sqrt(2) * sqrt(Pi) / exp(-1 * pow(p,2))
  }
}