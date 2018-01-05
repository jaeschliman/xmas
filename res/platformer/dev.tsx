<?xml version="1.0" encoding="UTF-8"?>
<tileset name="dev" tilewidth="32" tileheight="32" tilecount="64" columns="8">
 <image source="tiles.png" width="256" height="256"/>
 <tile id="0">
  <properties>
   <property name="anum" type="int" value="23"/>
   <property name="material" value="brick"/>
   <property name="shape" value="block"/>
  </properties>
 </tile>
 <tile id="1">
  <properties>
   <property name="material" value="brick"/>
   <property name="shape" value="slope-left"/>
  </properties>
 </tile>
 <tile id="2">
  <properties>
   <property name="material" value="brick"/>
   <property name="shape" value="slope-right"/>
  </properties>
 </tile>
 <tile id="8">
  <properties>
   <property name="material" value="ice"/>
   <property name="shape" value="block"/>
  </properties>
 </tile>
 <tile id="63">
  <properties>
   <property name="edge-case" type="int" value="23"/>
  </properties>
 </tile>
</tileset>
