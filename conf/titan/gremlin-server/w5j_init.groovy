
def globals = [:];

globals["hook"] = [
  onStartUp : { ctx ->
    makeMixedIndex = { prop_name, data_class -> 
      graph.tx().rollback();
      def mgmt = graph.openManagement();
      if(mgmt.getPropertyKey(prop_name) != null) return;
      mgmt.makePropertyKey(prop_name).dataType(data_class).make();
      mgmt.commit();

      def index_name = prop_name + "_index";
      mgmt = graph.openManagement();
      mgmt.buildIndex(index_name, Vertex.class).addKey(mgmt.getPropertyKey(prop_name)).buildMixedIndex("search");
      mgmt.commit();
      
      //Wait for the index to become available
      mgmt.awaitGraphIndexStatus(graph, index_name).call();

      // //Reindex the existing data
      // mgmt = graph.openManagement()
      // mgmt.updateIndex(mgmt.getGraphIndex("whenFromIndex"), SchemaAction.REINDEX).get()
      // mgmt.commit()
    };

    configTags = { ->
      graph.tx().rollback();
      def mgmt = graph.openManagement();
      if(mgmt.getPropertyKey("tags") != null) return;
      mgmt.makePropertyKey("tags").dataType(String.class).cardinality(Cardinality.SET).make();
      mgmt.commit();

      mgmt = graph.openManagement();
      mgmt.buildIndex("tags_index", Vertex.class).addKey(mgmt.getPropertyKey("tags")).buildCompositeIndex();
      mgmt.commit();
      mgmt.awaitGraphIndexStatus(graph, "tags_index").call();
    };

    configTags();
    makeMixedIndex("title", String.class);
    makeMixedIndex("body", String.class);
  }
] as LifeCycleHook

// define the default TraversalSource to bind queries to.
def g = globals["g"] = graph.traversal();

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

def toElem = { traversal ->
  def elems = traversal.next(1);
  if(elems.isEmpty()) {
    return null;
  }else {
    return elems[0];
  }
};

// def getOrCreateV = { id, _label ->
//   if(id == null) {
//     return graph.addVertex(_label);
//   }
//   return toElem(g.V(id).hasLabel(_label));
// };

def getOrCreateWhere = { props ->
  def v = toElem(g.V().hasLabel("where").has("where_name", props["where_name"]));
  if(v == null) {
    v = graph.addVertex("where");
    setProps(v, props, ["where_name"]);
  }
  return v;
};

def addWhen = globals["addWhen"] = { props ->
  def v = graph.addVertex("when");
  setProps(v, props, ["instant", "is_time_explicit", "time_zone"]);
  return v;
};

def addWhat = globals["addWhat"] = { props ->
  def v = graph.addVertex("what");
  setProps(v, props, ["title", "body", "created_at", "updated_at", "tags"])
  if(props["when"] != null) {
    def v_from = addWhen(props["when"]["from"]);
    def v_to = addWhen(props["when"]["to"]);
    v.addEdge("when_from", v_from);
    v.addEdge("when_to", v_to);
  }
  props["wheres"].each { where_prop ->
    def where_v = getOrCreateWhere(where_prop);
    v.addEdge("where", where_v);
  };
  return v;
};

def getCompleteWhat = globals["getCompleteWhat"] = { what_v ->
  return [what_v,
          __(what_v).out('when_from').toList(),
          __(what_v).out('when_to').toList(), 
          __(what_v).out('where').toList()];
};

def optionalT = globals["optionalT"] = { traversal ->
  return coalesce(traversal.map({ return Optional.of(it.get()); }), constant(Optional.empty()));
};

def compareOptWhenVertices = globals["compareOptWhenVertices"] = { opt_wa, opt_wb ->
  if(!opt_wa.isPresent() &&  opt_wb.isPresent()) return 1;
  if( opt_wa.isPresent() && !opt_wb.isPresent()) return -1;
  if(!opt_wa.isPresent() && !opt_wb.isPresent()) return 0;
  def wa = opt_wa.get();
  def wb = opt_wb.get();
  def comp_instant = wa.value("instant").compareTo(wb.value("instant"));
  if(comp_instant != 0) return comp_instant;
  return wa.value("is_time_explicit").compareTo(wb.value("is_time_explicit"));
} as Comparator;

// return bindings
globals;
