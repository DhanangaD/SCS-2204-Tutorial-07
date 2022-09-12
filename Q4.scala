case class Account(acc: String, bal: Double){
  var accountNo = acc
  var balance = bal      
            
  def isNegativeBalance() = if (balance < 0) true else false
  
  def addInterest(depositRate: Double, overdraftRate: Double) = balance = balance * (1 + (if(isNegativeBalance()) overdraftRate else depositRate))
  
  override def toString = "\nAccount : " + accountNo + "\nBalance : " + balance
            
}

object Bank extends App{
    var accounts: List[Account] = List(
    Account("001", -1000.0), 
    Account("002", 2000.0), 
    Account("003", 3000.0)
    )

    println("\n")
      println("List of Accounts with negative balances : ")
    val negativeBalances = accounts.filter(account => account.isNegativeBalance())
    negativeBalances.map(account => println(account))
    
    println("\n")
      println("The sum of all account balances : ")
    val total = accounts.reduce((x, y) => Account("-TOTAL-", x.balance + y.balance))
    println(total)
    
    println("\n")
      println("The final balances of all accounts after apply the interest : ")
    accounts.map(account => account.addInterest(0.05, 0.1))
    accounts.map(account => println(account))

}