
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ARFF;
with Openml; use Openml;

package body OML_File_Tests is

   pragma Warnings (Off);

   procedure Test_Features (Data_Id : Integer; Dataset : String := "");
   procedure Test_Qualities (Data_Id : Integer; Dataset : String := "");

   --  -------------------------------------------------------------------------

   procedure Test_Convert_Arff_To_Data is
--        Routine_Name   : constant String := "Test_Convert_Arff_To_Data ";
      File_Name      : constant String := "iris.arff";
      File           : File_Type;
      Data           : Unbounded_String := To_Unbounded_String ("");
      Result         : JSON_Value;
      --  Arff_Sparse_Data_Type is a subtype of JSON_Array
--        ARFF_Data      : ARFF.Arff_Sparse_Data_Type;
      Features       : JSON_Array;
      Data_Columns   : JSON_Array;
      Target_Columns : JSON_Array;
   begin
      Openml.Download_Data_To_Bunch
        (URL => "", File_Name => File_Name, Sparse => False, As_Frame => False,
         Features_List => Features, Data_Columns  => Data_Columns,
         Target_Columns => Target_Columns, Shape => (1, 1));

      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Data := Data & To_Unbounded_String (Get_Line (File));
         Data := Data & "\r\n";
      end loop;
      Close (File);

      Result := ARFF.Load (To_String (Data), ARFF.Arff_Dense);
--        Convert_Arff_To_Data (Dataset_Name, Version, Data_Id);

   end Test_Convert_Arff_To_Data;

   --  -------------------------------------------------------------------------

   procedure Test_Data_Description (Data_Id   : Integer;
                                    Dataset   : String := "") is
      Routine_Name  : constant String := "Test_Data_Description ";
      Description   : JSON_Value;
   begin
      New_Line;
      Description := Get_Data_Description_By_ID (Data_Id, Dataset);
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

   procedure Test_Features (Data_Id : Integer; Dataset : String := "") is
      Routine_Name   : constant String := "Test_Features ";
      Feature_Array  : JSON_Array;
      Index          : Positive;
      aFeature       : JSON_Value;
      Feature_Index  : Natural;
   begin
      Put_Line (Routine_Name & "Get features");
      Feature_Array := Get_Data_Features (Data_Id, File_Name => Dataset);
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

   procedure Test_Qualities (Data_Id : Integer; Dataset : String := "") is
      use ML_Qualities_Package;
      Routine_Name  : constant String := "Test_Qualities ";
      Count         : Natural := 0;
      Quality_Array : Qualities_Map;
      Curs          : Cursor;
      Quality       : JSON_Value;

      procedure Process_Quality (Value : JSON_Value) is
      begin
         Count := Count + 1;
         if Count > 20 and Count < 31 then
            Put ("Quality value:");
            case Kind (Value) is
            when JSON_Array_Type => Put_Line (" JSON_Array");
            when JSON_Boolean_Type =>
               Put_Line (Boolean'Image (Get (Value)));
            when JSON_Float_Type =>
               Put_Line (Float'Image (Get (Value)));
            when JSON_Int_Type =>
               Put_Line (Integer'Image (Get (Value)));
            when JSON_Null_Type => Put_Line (" Null");
            when JSON_Object_Type => Put_Line (" JSON_Object");
            when JSON_String_Type => Put_Line (Get (Value));
            end case;
         end if;

      end Process_Quality;

   begin
      Quality_Array := Get_Data_Qualities (Data_Id, Dataset_Name => Dataset);

      if Quality_Array.Is_Empty then
         Put_Line (Routine_Name & "there are no qualities");
      else
         Curs := Quality_Array.First;
         while Has_Element (Curs) loop
            Quality := Element (Curs);
            Process_Quality (Quality);
            Next (Curs);
         end loop;
      end if;
      New_Line;

   end Test_Qualities;

   --  -------------------------------------------------------------------------

end OML_File_Tests;
