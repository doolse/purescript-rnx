'use strict';

const RN = require("react-native")

exports.backAndroidAddListener = RN.BackAndroid.addEventListener
exports.drawerLayoutAndroidClass = RN.DrawerLayoutAndroid
exports.dcLeft = RN.DrawerLayoutAndroid.positions.Left
exports.dcRight = RN.DrawerLayoutAndroid.positions.Right
exports.openDrawer = function (this_) {
  return function() {
    this_.openDrawer();
  }
}
exports.closeDrawer = function (this_) {
  return function() {
    this_.closeDrawer();
  }
}

exports.exitApp = RN.BackAndroid.exitApp
