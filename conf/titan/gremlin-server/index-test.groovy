
hook = [
  onStartUp : { ctx ->
    makeIndexFor = { prop_name, data_class -> 
      graph.tx().rollback();
      mgmt = graph.openManagement();
      if(mgmt.getPropertyKey(prop_name) != null) return;

      index_name = prop_name + "_index";
      prop_key = mgmt.makePropertyKey(prop_name).dataType(data_class).make()
      mgmt.buildIndex(index_name, Vertex.class).addKey(prop_key).buildMixedIndex("search")
      mgmt.commit()
      
      //Wait for the index to become available
      mgmt.awaitGraphIndexStatus(graph, index_name).call()

      // //Reindex the existing data
      // mgmt = graph.openManagement()
      // mgmt.updateIndex(mgmt.getGraphIndex("whenFromIndex"), SchemaAction.REINDEX).get()
      // mgmt.commit()
    };

    configTags = { ->
      mgmt = graph.openManagement();
      if(mgmt.getPropertyKey("tags") != null) return;
      mgmt.makePropertyKey("tags").dataType(String.class).cardinality(Cardinality.SET).make();
      mgmt.commit();
    };

    makeIndexFor("when_from", Instant.class);
    configTags();

  }
] as LifeCycleHook

// define the default TraversalSource to bind queries to.
g = graph.traversal()
