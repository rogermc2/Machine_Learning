--  State machine for recognizing and storing Dot input
with Ada.Exceptions;
with Ada.Text_IO;

with Dot_Tables;
with Types; use Types;
with Utilities;

package body State_Machine is

   Current_State     : State := Digraph;
   Current_Statement : Statement;
   Current_Attribute : Dot_Tables.Attribute;
   Current_Name      : Config.Name;

   procedure Parse_Line (Table : in out Dot_Tables.Table_Data;
                         N     : Config.Name) is
      use Utilities;
      anElement : Types.Elements.Element;
   begin
      case Current_State is
         --  First line should be "digraph name {"
         when Digraph =>
            if N = Utilities.Pad ("digraph") then
               Current_State := Name;
            else
               raise Syntax_Error with "digraph";
            end if;

         when Name =>
            Table.Graph_Name := N;
            Current_State := Open_Brace;

         when Open_Brace =>
            if N = Pad ("{") then
               Current_State := Statements;
            else
               raise Syntax_Error with "{";
            end if;

         when Statements =>
            --  Statement
            --   graph | node | edge [ list of key = value ];
            if N = Pad ("}") then
               --  end of input
               null;
            elsif N = Pad ("graph") then
               --  Three possible attributes
               Current_Statement := Attributes;
               Current_Attribute := Dot_Tables.Graphs;
               Current_State := Open_Bracket;

            elsif N = Pad ("node") then
               --  Node: store name and look for edge pointer
               Current_Statement := Attributes;
               Current_Attribute := Dot_Tables.Nodes;
               Current_State := Open_Bracket;

            elsif N = Pad ("edge") then
               --  Edge: store name and look for edge pointer
               Current_Statement := Attributes;
               Current_Attribute := Dot_Tables.Edges;
               Current_State := Open_Bracket;

            else
               Current_Name := N;
               Current_State := Pointer;
            end if;

         when Pointer =>
            if N = Pad ("->") then
               --  Edge pointer so look for target
               Current_Statement := Edges;
               Current_State := Targets;
            else
               --  Must be a node, store in element vector
               Element_Vectors.Append
                 (Table.Nodes, (Source => Current_Name, Target => <>,
                                Attributes => <>));
               Current_Statement := Nodes;
               if N = Pad ("[") then
                  --  Attribute list after node
                  Current_State := Attributes;
               elsif N = Pad (";") then
                  Current_State := Statements;
               else
                  raise Syntax_Error with "[ or ;";
               end if;
            end if;

         when Targets =>
            --  Target of an edge, store in element vector
            Element_Vectors.Append
              (Table.Edges,
               (Source => Current_Name, Target => N, Attributes => <>));
            Current_State := Open_Bracket;

         when Open_Bracket =>
            --  Get attribute list
            --  List opens with bracket
            if N = Pad ("[") then
               Current_State := Attributes;
            elsif N = Pad (";") then
               Current_State := Statements;
            else
               raise Syntax_Error with "[ or ;";
            end if;

            --  List closes with a bracket; otherwise, save key
         when Attributes =>
            if N = Pad ("]") then
               --  List closes with a bracket
               Current_State := Semicolon;
            else
               --  otherwise, save key
               Current_Name := N;
               Current_State := Equals;
            end if;

         when Equals =>
            if N = Pad ("=") then
               Current_State := Values;
            else
               raise Syntax_Error with "=";
            end if;

         when Values =>
            --  Value found, insert key and value in attribute list
            if Current_Statement = Attributes then
               --  graph | node | edge attribute list
               Attribute_Maps.Insert
                 (Table.Attribute_Map_Array (Current_Attribute),
                  Current_Name, N);

            elsif Current_Statement = Nodes then
               --  Attribute list is associated with a node
               --  Put list in last element found
               anElement := Element_Vectors.Last_Element (Table.Nodes);
               Attribute_Maps.Insert (anElement.Attributes,
                                      Current_Name, N);
               Element_Vectors.Replace_Element
                 (Table.Nodes, Element_Vectors.Last (Table.Nodes), anElement);

            elsif Current_Statement = Edges then
               --  Attribute list is associated with an edge
               --  Put list in last element found
               anElement := Element_Vectors.Last_Element (Table.Edges);
               Attribute_Maps.Insert (anElement.Attributes, Current_Name, N);
               Element_Vectors.Replace_Element
                 (Table.Edges, Element_Vectors.Last (Table.Edges), anElement);
            end if;

            Current_State := Attributes;

         when Semicolon =>
            if N = Pad (";") then
               --  End of statement
               Current_State := Statements;
            else
               raise Syntax_Error with ";";
            end if;
      end case;

   exception
      when E : Syntax_Error =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E) &
                                 " expected");
      when others =>
         Ada.Text_IO.Put_Line ("State_Machine exception");
   end Parse_Line;

end State_Machine;
