
with Ada.Text_IO; use Ada.Text_IO;

package body Export_Printing is

   --  -------------------------------------------------------------------------

   procedure Print_Bounds (Name : String; Data : Export_Types.Bounds_List) is
      use Export_Types;
      Bound : Graph_Bounds;
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         Bound := Data.Element (Index);
         Put ("(" & Float'Image (Bound.H) & ", " & Float'Image (Bound.V) & ")");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Bounds;

   --  ------------------------------------------------------------------------

   procedure Print_Colours_List (Name    : String;
                                 Colours : Export_Types.Colours_List) is
      use Export_Types.Colours_Package;
      Item : Export_Types.Graph_Colours;
   begin
      Put_Line (Name & " RGB: ");
      for row in Colours.First_Index .. Colours.Last_Index loop
         Item := Colours.Element (row);
         Put_Line (Integer'Image (row)  & ", " & Float'Image (Item.R) &
                     ", " & Float'Image (Item.G) & ", " &
                     Float'Image (Item.B));
      end loop;
      New_Line;

   end Print_Colours_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Colours_List
     (Name : String; Colours : Export_Types.Integer_Colours_List) is
      use Export_Types.Integer_Colours_Package;
      Item : Export_Types.Integer_Graph_Colours;
   begin
      Put_Line (Name & " RGB: ");
      for row in Colours.First_Index .. Colours.Last_Index loop
         Item := Colours.Element (row);
         Put_Line (Natural'Image (Item.R) & ". " & Natural'Image (Item.G) &
                     ", " & Natural'Image (Item.B));
      end loop;
      New_Line;

   end Print_Integer_Colours_List;

   --  ------------------------------------------------------------------------

   procedure Print_Export_Map
     (Name : String; aMap : Export_Types.Export_Map) is
      use Export_Types;
      use Export_Types.Export_Maps;
      Curs : Cursor := aMap.First;
      aKey : Unbounded_String;
   begin
      Put_Line (Name & ": ");
      while Has_Element (Curs) loop
         aKey := Key (Curs);
         Put_Line (To_String (aKey) & ":  " & To_String (Element (Curs)));
         Next (Curs);
      end loop;
      New_Line;
   end Print_Export_Map;

   --  ------------------------------------------------------------------------

   procedure Print_RGB_Array (Name    : String;
                              anArray : Export_Types.RGB_Array) is
      use Export_Types;
      Colours : Graph_Colours;
   begin
      Put_Line (Name & ": ");
      for index in anArray'First .. anArray'Last loop
         Colours := anArray (index);
         Put (Float'Image (Colours.R) & ", ");
         Put (Float'Image (Colours.G) & ", ");
         Put_Line (Float'Image (Colours.B));
      end loop;

   end Print_RGB_Array;

   --  ------------------------------------------------------------------------

end Export_Printing;
