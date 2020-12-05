package dev.czora.aoc2020

object Main {
  val data =
    "1826\n1895\n1427\n1931\n1651\n1638\n1507\n1999\n1886\n1824\n1902\n1995\n1945\n1735\n1823\n1595\n1936\n1476\n2010\n1833\n1932\n1772\n1791\n1814\n1783\n1957\n1901\n1600\n1502\n1521\n1812\n1974\n1880\n1790\n1672\n1541\n1807\n426\n1858\n1699\n1964\n1996\n1983\n1498\n1863\n1976\n1492\n1930\n1838\n1941\n1764\n1929\n1897\n2009\n1853\n1753\n1759\n1860\n1952\n1988\n1727\n1751\n1943\n1830\n1645\n1907\n1857\n1714\n1798\n1944\n1868\n1630\n959\n2003\n1987\n1890\n1962\n1928\n1872\n1912\n1709\n1809\n1650\n1980\n1737\n1898\n1817\n1736\n1991\n1788\n1776\n1845\n1854\n1963\n1554\n1949\n1576\n1819\n1960\n699\n1990\n1757\n1698\n1596\n304\n1982\n1477\n1961\n1636\n1619\n1946\n1876\n1592\n1848\n1707\n1958\n1874\n1867\n52\n1881\n1665\n1463\n1799\n1979\n677\n1710\n1869\n1639\n1787\n1633\n1956\n1457\n1581\n2005\n1782\n1904\n1910\n62\n1593\n1695\n1915\n1922\n1797\n1715\n1981\n1925\n1893\n1562\n1789\n2008\n1939\n1669\n1992\n1652\n117\n1609\n1686\n1953\n2007\n599\n1547\n1959\n1691\n1520\n1444\n1641\n887\n1579\n1778\n1977\n1768\n1942\n1713\n1603\n1926\n1855\n1655\n1673\n1887\n1994\n1839\n1725\n928\n1771\n1761\n1971\n1571\n1806\n1821\n1624\n1701\n1436\n1748\n1921\n1617\n2004\n1792\n1732\n1740\n1831"
  def main(args: Array[String]): Unit = {

    val lines = data.split('\n').map(l => l.toInt).toList

    // Part 1
    val partners = findPartners(2020, lines)
    val results = partners.map { case (a, b) => a * b }
    println(results)

    // Part 2
    val triplePartners = findTriplePartners(2020, lines)
    val resultsForTriplePartners = triplePartners.map {
      case (a, b, c) => a * b * c
    }
    println(resultsForTriplePartners)

  }

  def findPartners(sum: Int, list: List[Int]): List[(Int, Int)] = {
    list match {
      case Nil => Nil
      case x :: xs =>
        xs.filter(p => p + x == sum).map(i => (i, x)) ::: findPartners(sum, xs)
    }
  }

  def findTriplePartners(sum: Int, list: List[Int]): List[(Int, Int, Int)] = {
    list match {
      case Nil => Nil
      case x :: xs =>
        findPartners(sum - x, xs).map {
          case (a, b) => (a, b, x)
        } ::: findTriplePartners(sum, xs)
    }
  }

  // Instead of having one function for each partner set size (up to n), it would be nicer to have one general function

}
