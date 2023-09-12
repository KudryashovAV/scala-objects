package objsets

class TweetSetSuite extends munit.FunSuite:
  trait TestSets:
    val set0: Empty = Empty()
    val set1: TweetSet = set0.incl(Tweet("a", "a body", 20))
    val set2: TweetSet = set1.incl(Tweet("b", "b body", 20))
    val userC: Tweet = Tweet("c", "c body", 7)
    val userD: Tweet = Tweet("d", "d body", 21)
    val set3c: TweetSet = set2.incl(userC)
    val set3d: TweetSet = set2.incl(userD)
    val set4: TweetSet = set3c.incl(userD)

  def asSet(tweets: TweetSet): Set[Tweet] =
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res

  def size(set: TweetSet): Int = asSet(set).size

  test("filter empty set by user value"):
    new TestSets:
      assertEquals(size(set0.filter(tw => tw.user == "a")), 0)

  test("filter by user value"):
    new TestSets:
      assertEquals(size(set4.filter(tw => tw.user == "a")), 1)

  test("filter by retweets value"):
    new TestSets:
      assertEquals(size(set4.filter(tw => tw.retweets == 20)), 2)

  test("union two almost identity sets"):
    new TestSets:
      assertEquals(size(set3c.union(set3d)), 4)

  test("union non empty set with empty set"):
    new TestSets:
      assertEquals(size(set4.union(set0)), 4)

  test("union empty set with non empty"):
    new TestSets:
      assertEquals(size(set0.union(set4)), 4)

  test("returns most retweeted twit"):
    new TestSets:
      val mostRetweetedTwit: Tweet = set4.mostRetweeted
      assert(mostRetweetedTwit.text == "d body")

  test("returns most retweeted twit in the head of list"):
    new TestSets:
      val trends: TweetList = set4.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "d")

  import scala.concurrent.duration.*

  override val munitTimeout: FiniteDuration = 10.seconds
