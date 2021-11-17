
with Ada.Text_IO;

with Export_Types; use Export_Types;
with Config;

package Dot_Tables is
        type Attribute is (Graphs, Nodes, Edges);
        type Attribute_Map_Arrays is array (Attribute) of Attribute_Maps.Map;

        type Table_Data is record
            Graph_Name          : Config.Name;
            Attribute_Map_Array : Attribute_Map_Arrays;
            Nodes               : Element_Vectors.Vector;
            Edges               : Element_Vectors.Vector;
        end record;

        procedure Put (T : Table_Data; Output  : Ada.Text_IO.File_Type);
        procedure Sort (T : in out Table_Data);

end Dot_Tables;
