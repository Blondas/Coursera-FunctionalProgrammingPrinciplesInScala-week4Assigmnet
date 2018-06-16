val list = List(1,2,3,1,4,1,2)
list.groupBy(identity).mapValues(_ .size).toList