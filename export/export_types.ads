
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

package Export_Types is

   --  A statement is a list of attributes or an element (node or edge)
   type Statement is (Attributes, Nodes, Edges);
   --  States for recognizer
   type State is
     (Digraph, Name, Open_Brace, Statements, Open_Bracket, Equals, Semicolon,
      Attributes, Values, Pointer, Targets);

   Syntax_Error : exception;

   type Graph_Colours is record
      R : Float := 0.0;
      G : Float := 0.0;
      B : Float := 0.0;
   end record;

   type Integer_Graph_Colours is record
      R : Natural := 0;
      G : Natural := 0;
      B : Natural := 0;
   end record;

   type Graph_Bounds is record
      H : Float := 0.0;
      V : Float := 0.0;
   end record;

   package Colours_Package is new Ada.Containers.Vectors
     (Positive, Graph_Colours);
   subtype Colours_List is Colours_Package.Vector;

   package Integer_Colours_Package is new Ada.Containers.Vectors
     (Positive, Integer_Graph_Colours);
   subtype Integer_Colours_List is Integer_Colours_Package.Vector;

   package Bounds_Package is new Ada.Containers.Vectors
     (Positive, Graph_Bounds);
   subtype Bounds_List is Bounds_Package.Vector;

   use Colours_Package;
   package Export_Colour_Maps is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Colours_List);
   subtype Colour_Map is Export_Colour_Maps.Map;

   package Export_Maps is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   subtype Export_Map is Export_Maps.Map;

end Export_Types;
