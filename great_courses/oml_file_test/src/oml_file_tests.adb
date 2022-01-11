
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_File_Tests is

   procedure Test_Features (Data_Id : Integer; File_Name : String := "");
   procedure Test_Qualities (Data_Id : Integer; File_Name : String := "");

   --  -------------------------------------------------------------------------

   procedure Test_Data_Description (Data_Id   : Integer;
                                    File_Name : String := "") is
      Routine_Name  : constant String := "Test_Data_Description ";
      Description   : JSON_Value;
   begin
      New_Line;
      Description := Get_Data_Description_By_ID (Data_Id, File_Name);
      declare
         Desc : constant String := Get (Description);
      begin
         Put_Line (Routine_Name & "Description length:" &
                     Integer'Image (Desc'Length) & " characters.");
         Put_Line (Routine_Name & "Description:");
         --           Put_Line (Desc);
      end;
      New_Line;

   end Test_Data_Description;

   --  -------------------------------------------------------------------------

   procedure Test_Data_Info is
      Routine_Name       : constant String := "Test_Data_Info ";
      Dataset_Name       : constant String := "mnist_784";
      Info_File_Name     : constant String := "mnist_784";
      Features_File_Name : constant String := "features";
      Version            : constant String := "1";
      Data_Info          : JSON_Value;
      JSON_Data_Id       : JSON_Value;
      Data_Id            : Integer := 0;
   begin
      New_Line;
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version,
                                          File_Name => Info_File_Name);
      JSON_Data_Id := Get (Data_Info, "data_id");
      Data_Id := Integer'Value (Get (JSON_Data_Id));
      Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));

      Test_Data_Description (Data_Id, Info_File_Name);
      Test_Features  (Data_Id, Features_File_Name);
      Test_Qualities (Data_Id, Info_File_Name);

   end Test_Data_Info;

   --  -------------------------------------------------------------------------

   procedure Test_Features (Data_Id : Integer; File_Name : String := "") is
      Routine_Name   : constant String := "Test_Features ";
      Feature_Array  : JSON_Array;
      Index          : Positive;
      aFeature       : JSON_Value;
      Feature_Index  : Natural;
   begin
      Put_Line (Routine_Name & "Get features");
      Feature_Array := Get_Data_Features (Data_Id, File_Name => File_Name);
      Index := Array_First (Feature_Array);
      while Array_Has_Element (Feature_Array, Index) loop
         aFeature := Array_Element (Feature_Array, Index);
         Feature_Index := Integer'Value (Get (aFeature, "index"));
         if Feature_Index < 4 then
            Put_Line ("aFeature index: " & Integer'Image (Feature_Index));
         end if;
         --           Put_Line ("aFeature JSON type: " &
         --                       JSON_Value_Type'Image (kind (aFeature)));
         Index := Array_Next (Feature_Array, Index);
      end loop;

      New_Line;

   end Test_Features;

   --  -------------------------------------------------------------------------

   procedure Test_Fetch_OML is
      Routine_Name  : constant String := "Test_Fetch_OML ";
      Dataset_Name  : constant String := "mnist_784";
      Version       : constant String := "1";
      Data_Id       : Integer := 0;
   begin
      Fetch_Openml (Dataset_Name, Version, Data_Id);
      Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));

   end Test_Fetch_OML;

   --  -------------------------------------------------------------------------

   procedure Test_Qualities (Data_Id   : Integer; File_Name : String := "") is
      Routine_Name  : constant String := "Test_Qualities ";
      Index         : Positive;
      Quality_Array : JSON_Array;
      Quality       : JSON_Value;

      procedure Process_Quality (Name : Utf8_String; Value : JSON_Value) is
      begin
         if Index < 11 then
            Put (Name & ": ");
            case Kind (Value) is
            when JSON_Array_Type => Put_Line ("JSON_Array");
            when JSON_Boolean_Type =>
               Put_Line (Boolean'Image (Get (Value)));
            when JSON_Float_Type =>
               Put_Line (Float'Image (Get (Value)));
            when JSON_Int_Type =>
               Put_Line (Integer'Image (Get (Value)));
            when JSON_Null_Type => null;
            when JSON_Object_Type => Put_Line ("JSON_Object");
            when JSON_String_Type => Put_Line (Get (Value));
            end case;
         end if;
      end Process_Quality;

   begin
      Quality_Array := Get_Data_Qualities (Data_Id, File_Name => File_Name);

      if Is_Empty (Quality_Array) then
         Put_Line (Routine_Name & "there are no qualities");
      else
         Index := Array_First (Quality_Array);
         while Array_Has_Element (Quality_Array, Index) loop
            Quality := Array_Element (Quality_Array, Index);
            Map_JSON_Object (Quality, Process_Quality'Access);
            Index := Array_Next (Quality_Array, Index);
         end loop;
      end if;

      New_Line;

   end Test_Qualities;

   --  -------------------------------------------------------------------------

end OML_File_Tests;
