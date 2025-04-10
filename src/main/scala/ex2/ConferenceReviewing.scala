package ex2

import java.util

/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the 
 * system does not keep track of the identity of reviewers
 *
 */
object ConferenceReviewing {
  /**
   * For each article, the reviewer has to reply to all the following questions
   */
  enum Question {
    case RELEVANCE    // ("È importante per questa conferenza?"),
    case SIGNIFICANCE // ("Produce contributo scientifico?"),
    case CONFIDENCE   // ("Ti senti competente a commentarlo?");
    case FINAL        // ("É un articolo da accettare?")
  }
}

trait ConferenceReviewing {
  import ConferenceReviewing.*
  
  /**
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: util.Map[Question, Integer]): Unit

  /**
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list 
   */
  def orderedScores(article: Int, question: Question): util.List[Integer]

  /**
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accepted if its averageFinalScore (not weighted) is > 5, 
   * and at least one RELEVANCE score that is >= 8.
   * @return the set of accepted articles
   */
  def acceptedArticles: List[Integer]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[Nothing]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10  
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: util.Map[Integer, Double]
}

class ConferenceReviewingImpl extends ConferenceReviewing:
  import ConferenceReviewing.Question
  override def loadReview(article: Int, scores: util.Map[Question, Integer]): Unit = ???
  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = ???
  override def orderedScores(article: Int, question: Question): util.List[Integer] = ???
  override def averageFinalScore(article: Int): Double = ???
  override def acceptedArticles: List[Integer] = ???
  override def sortedAcceptedArticles: List[Nothing] = ???
  override def averageWeightedFinalScoreMap: util.Map[Integer, Double] = ???