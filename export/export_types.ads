
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Bounded_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Config;

package Export_Types is

   --  A statement is a list of attributes or an element (node or edge)
   type Statement is (Attributes, Nodes, Edges);
   --  States for recognizer
   type State is
     (Digraph, Name, Open_Brace, Statements, Open_Bracket, Equals, Semicolon,
      Attributes, Values, Pointer, Targets);

   Syntax_Error : exception;

   package Lines is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Config.Line_Length);

   package Lines_IO is new Ada.Text_IO.Bounded_IO (Lines);

   package Attribute_Maps is new
     Ada.Containers.Ordered_Maps (Config.Name, Config.Name);

   package Export_Maps is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
   subtype Export_Map is Export_Maps.Map;

   package Elements is
      Blanks : constant Config.Name := (others => ' ');
      type Element is record
         Source     : Config.Name;
         Target     : Config.Name := Blanks;
         Attributes : Attribute_Maps.Map := Attribute_Maps.Empty_Map;
      end record;

      function "=" (Left, Right : Element) return Boolean;
      function "<" (Left, Right : Element) return Boolean;

   end Elements;

   use Elements;
   package Element_Vectors is new
     Ada.Containers.Vectors (Natural, Elements.Element);

   package Tables is
      type Attribute is (Graphs, Nodes, Edges);
      type Attribut_Map_Arrays is array (Attribute) of Attribute_Maps.Map;

      type Table is record
         Graph_Name         : Config.Name;
         Attribut_Map_Array : Attribut_Map_Arrays;
         Nodes              : Element_Vectors.Vector;
         Edges              : Element_Vectors.Vector;
      end record;

   end Tables;

end Export_Types;
