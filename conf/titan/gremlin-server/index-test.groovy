
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

def setProps = { elem, props, keys ->
  keys.each { key ->
    def val = props[key];
    if(val instanceof List) {
      // LIST/SET cardinality
      def ex_prop = elem.property(key);
      if(ex_prop != null) {
        ex_prop.remove();
      }
      val.each { v ->
        elem.property(key, v);
      };
      return;
    }
    elem.property(key, val);
  };
};

def addWhen = globals["addWhen"] = { props ->
  def v = graph.addVertex(label, "when");
  setProps(v, props, ["instant", "is_time_explicit", "time_zone"]);
  return v;
};

def addWhat = globals["addWhat"] = { props ->
  def v = graph.addVertex(label, "what");
  setProps(v, props, ["title", "body", "created_at", "updated_at", "tags"])
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
