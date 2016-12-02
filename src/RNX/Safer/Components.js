'use strict';
const RN = require("react-native");

const ListView = RN.ListView;

exports.listViewDataSource = function(dict){
  return function(items){
    return new ListView.DataSource({
      rowHasChanged: function(r1, r2){ return !dict.eq(r1)(r2); }
    }).cloneWithRows(items);
  }
}

exports.cloneWithRows = function(ds) {
  return function (data) {
    return ds.cloneWithRows(data);
  }
}
