
hook = [
  onStartUp : { ctx ->
    graph.tx().rollback()  //Never create new indexes while a transaction is active
    mgmt = graph.openManagement()
    if(mgmt.getPropertyKey('when_from') != null) return;
    
    when_from = mgmt.makePropertyKey('when_from').dataType(Instant.class).make()
    mgmt.buildIndex('whenFromIndex', Vertex.class).addKey(when_from).buildMixedIndex("search")
    mgmt.commit()
    
    //Wait for the index to become available
    mgmt.awaitGraphIndexStatus(graph, 'whenFromIndex').call()
    
    // //Reindex the existing data
    // mgmt = graph.openManagement()
    // mgmt.updateIndex(mgmt.getGraphIndex("whenFromIndex"), SchemaAction.REINDEX).get()
    // mgmt.commit()
  }
] as LifeCycleHook

// define the default TraversalSource to bind queries to.
g = graph.traversal()
