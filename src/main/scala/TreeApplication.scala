
/**
  * @param lhs Left Node
  * @param rhs Right Node
  * @param value value of the node
  */
case class Node(var lhs : Node, var rhs : Node, var value: Int) {

}

/**
  *
  * @param root
  *
  *  The insert() Method inserts specific value in the tree
  *  The searchNode() Method searches for the specific node by the value
  */
class Tree(var root: Node) {
  def this() = this(null)

  def insert(value: Int) : Unit = {
    if (root == null) {
      root = new Node(null, null, value)
    } else {
      insert(root, value)
    }
  }

  def insert(parent: Node, value : Int) : Unit = {
    if (value <= parent.value) {
      if (parent.lhs == null) {
        parent.lhs = new Node(null, null, value)
      } else {
        insert(parent.lhs, value)
      }
    } else {
      if (parent.rhs == null) {
        parent.rhs = new Node(null, null, value)
      } else {
        insert(parent.rhs, value)
      }
    }
  }

  def searchNode(valueToSearch: Int) : Node = {
    var node : Node = null

    if (root == null) {
      return null
    }

    if (root.value == null) {
      return null
    }

    if (root.value == valueToSearch) {
      return root
    } else if (valueToSearch < root.value) {
      node = searchNode(root.lhs, valueToSearch)
    } else {
      node = searchNode(root.rhs, valueToSearch)
    }

    return node
  }

  def searchNode(parent: Node, valueToSearch: Int) : Node = {
    var node : Node = null

    if (parent == null) {
      return null
    }

    if (parent.value == null) {
      return null
    }

    if (parent.value == valueToSearch) {
      return parent
    } else if (valueToSearch < parent.value) {
      return searchNode(parent.lhs, valueToSearch)
    } else {
      return searchNode(parent.rhs, valueToSearch)
    }
  }
}


/**
  * This Application is able to save numbers in the tree.
  * It is also able to retrieve numbers from the tree.
  */
object TreeApplication {

  def main(args : Array[String]) : Unit = {

    var programToExit: Boolean = false

    var baum : Tree = new Tree(null)

    do {
      println("----------------------------------------------")
      println("Menu: ")
      println("0: Insert value in the tree")
      println("1: search value")
      println("2: Finish")
      println("----------------------------------------------")
      print("Please choose an option(0-2): ")
      var number : Int = scala.io.StdIn.readInt()
      programToExit = matchOption(baum, number)
    } while(!programToExit)
  }

  def matchOption(tree: Tree, option: Int) : Boolean = option match {
    case 0 => {
      insertNumber(tree)
      return false
    }
    case 1 => {
      searchNumber(tree)
      return false
    }
    case 2 => return true
  }

  def insertNumber(tree : Tree) : Unit = {
    println("Inserting Number: ")
    print("Please type in the number which you want to save: ")
    val number : Int = scala.io.StdIn.readInt()
    tree.insert(number)
  }

  def searchNumber(tree: Tree) : Unit = {
    println("Searching Number: ")
    print("Please type in the number which you want to search: ")
    val number : Int = scala.io.StdIn.readInt()
    val node : Node = tree.searchNode(number)

    if (node == null) {
      println("Number not found.")
    } else {
      println("The Number " + node.value + " is available in the tree.")
    }
  }
}