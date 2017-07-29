
def globals = [:];

globals["hook"] = [
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

    configTags();

  }
] as LifeCycleHook

// define the default TraversalSource to bind queries to.
def g = globals["g"] = graph.traversal()

def addWhen = globals["addWhen"] = { props ->
  def v = graph.addVertex(label, "when");
  ["instant", "is_time_explicit", "time_zone"].each { k ->
    v.property(k, props[k]);
  };
  return v;
};

def addWhat = globals["addWhat"] = { props ->
  def v = graph.addVertex(label, "what");
  ["title", "body", "created_at", "updated_at"].each { k ->
    v.property(k, props[k]);
  };
  if(props["when"] != null) {
    def v_from = addWhen(props["when"]["from"]);
    def v_to = addWhen(props["when"]["to"]);
    v.addEdge("when_from", v_from);
    v.addEdge("when_to", v_to);
  }
  return v;
};

// return bindings
globals;
