object Main{
	import scala.collection.mutable.ListBuffer
	case class TreeNode[T](data:T,children:Seq[TreeNode[T]]=Nil)

	def asciiDisplay(root:TreeNode[String]):Seq[String]={
		val res:ListBuffer[String]=ListBuffer[String]("+-"+root.data)//首先把第一个节点记录
		def ForChildren(children:Seq[TreeNode[String]],prefix:String):Unit = {
			if(children==Nil) return
			
			if(children.length>1){
				for(child<-children){
					res+=prefix+"+-"+child.data
					ForChildren(child.children,prefix+"| ")
				}
				res+=prefix//若有多个子节点的情况下，在最后一个添加空行
			}
			else{
				for(child<-children){
					res+=prefix+"+-"+child.data
					ForChildren(child.children,prefix+"  ")
				}
			}
		}
		ForChildren(root.children,"  ")
		return res
	}

	def main(args: Array[String]): Unit = {
		asciiDisplay(TreeNode("Root",
      		children = List(TreeNode("level1-1"),
        	TreeNode("level1-2"),
        	TreeNode("level1-3")))).foreach(println)

		asciiDisplay(TreeNode("Root",
  			children = List(
    		TreeNode("level1-1", children = List(TreeNode("level2-1", children = List(TreeNode("level3-1"),TreeNode("level3-2"))),TreeNode("level2-2"))),
    		TreeNode("level1-2"),
    		TreeNode("level1-3")))).foreach(println)
	}
}