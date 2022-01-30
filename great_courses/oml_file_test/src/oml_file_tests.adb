
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_File_Tests is

--     pragma Warnings (Off);

   procedure Test_Features (Data_Id : Integer; Dataset : String := "");
   procedure Test_Qualities (Data_Id : Integer; Dataset : String := "");

   --  -------------------------------------------------------------------------

   procedure Test_Convert_Arff_To_Data is
      Routine_Name       : constant String := "Test_Convert_Arff_To_Data ";
      Features_File_Name : constant String := "diabetes_features";
      Data_Id            : constant Integer := 0;
      File_Name          : constant String := "diabetes.arff";
      Features           : constant JSON_Array :=
                             Get_Data_Features (Data_Id, Features_File_Name);
      Data_Age           : constant JSON_Value := Create_Object;
      Data_Sex           : constant JSON_Value := Create_Object;
      Data_BMI           : constant JSON_Value := Create_Object;
      Data_BP            : constant JSON_Value := Create_Object;
      Data_S1            : constant JSON_Value := Create_Object;
      Data_S2            : constant JSON_Value := Create_Object;
      Data_S3            : constant JSON_Value := Create_Object;
      Data_S4            : constant JSON_Value := Create_Object;
      Data_S5            : constant JSON_Value := Create_Object;
      Data_S6            : constant JSON_Value := Create_Object;
      Target             : constant JSON_Value := Create_Object;
      Feature_Columns    : JSON_Array;
      Data_Columns       : JSON_Array;
      Target_Columns     : JSON_Array;
      Bunch              : Bunch_Data;
   begin
      Put_Line (Routine_Name);
      Data_Age.Set_Field ("target", "age");
      Append (Feature_Columns, Data_Age);
      Data_Sex.Set_Field ("target", "sex");
      Append (Feature_Columns, Data_Sex);
      Data_BMI.Set_Field ("target", "bmi");
      Append (Feature_Columns, Data_BMI);
      Data_Bp.Set_Field ("target", "bp");
      Append (Feature_Columns, Data_Bp);
      Data_S1.Set_Field ("target", "s1");
      Append (Feature_Columns, Data_S1);
      Data_S2.Set_Field ("target", "s2");
      Append (Feature_Columns, Data_S2);
      Data_S3.Set_Field ("target", "s2");
      Append (Feature_Columns, Data_S3);
      Data_S4.Set_Field ("target", "s4");
      Append (Feature_Columns, Data_S4);
      Data_S5.Set_Field ("target", "s5");
      Append (Feature_Columns, Data_S5);
      Data_S6.Set_Field ("target", "s6");
      Append (Feature_Columns, Data_S6);

      Target.Set_Field ("target", "class");
      Append (Target_Columns, Target);

      Data_Columns := Valid_Data_Column_Names (Features, Feature_Columns);
      Target_Columns := Valid_Data_Column_Names (Features, Target_Columns);
      Assert (not Is_Empty (Data_Columns), Routine_Name &
              "Data_Columns is empty");
      Assert (not Is_Empty (Target_Columns), Routine_Name &
              "Target_Columns is empty");

      Bunch := Openml.Download_Data_To_Bunch
        (URL => "", File_Name => File_Name, Sparse => False, As_Frame => False,
         Features_List => Features, Data_Columns  => Data_Columns,
         Target_Columns => Target_Columns);  --  , Shape => (1, 1));

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
         Put_Line (Desc);
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
