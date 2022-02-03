
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_File_Tests is

   --     pragma Warnings (Off);

   procedure Test_Features (Data_Id : Integer;  Use_Files : Boolean := True);
--     Dataset : String := "");
   procedure Test_Qualities (Data_Id : Integer; Use_Files : Boolean := True);
--     Dataset : String := "");

   --  -------------------------------------------------------------------------

   procedure Test_Convert_Arff_To_Data is
      Routine_Name       : constant String := "Test_Convert_Arff_To_Data ";
--        Features_File_Name : constant String := "../diabetes_features";
      Data_Id            : constant Integer := 0;
--        File_Name          : constant String := "../diabetes";
      Features           : constant JSON_Array := Get_Data_Features (Data_Id);
--                               Get_Data_Features (Data_Id, Features_File_Name);
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
        (URL => "", Use_Files => True, Sparse => False, As_Frame => False,
         Features_List => Features, Data_Columns  => Data_Columns,
         Target_Columns => Target_Columns);  --  , Shape => (1, 1));

   end Test_Convert_Arff_To_Data;

   --  -------------------------------------------------------------------------

   procedure Test_Data_Description (Data_Id   : Integer;
                                    Use_Files : Boolean := True) is
--                                      Dataset   : String := "") is
      Routine_Name  : constant String := "Test_Data_Description ";
      Description   : JSON_Value;
   begin
      New_Line;
      Description := Get_Data_Description_By_ID (Data_Id, Use_Files);  --  , Dataset);
      declare
         Desc : constant String := Get (Description, "description");
      begin
         Put_Line (Routine_Name & "Description length:" &
                     Integer'Image (Desc'Length) & " characters.");
         --           Put_Line (Routine_Name & "Description:");
         --           Put_Line (Desc);
      end;
      New_Line;

   end Test_Data_Description;

   --  -------------------------------------------------------------------------

   procedure Test_Data_Info is
      Routine_Name       : constant String := "Test_Data_Info ";
      Dataset_Name       : constant String := "mnist_784";
--        Info_File_Name     : constant String := "../mnist_784";
--        Features_File_Name : constant String := "../features";
      Version            : constant String := "1";
      Data_Info          : JSON_Value;
      JSON_Data_Id       : JSON_Value;
      Data_Id            : Integer := 0;
   begin
      New_Line;
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
--                                            File_Name => Info_File_Name);
      JSON_Data_Id := Get (Data_Info, "data_id");
      Data_Id := Integer'Value (Get (JSON_Data_Id));
      Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));

      Test_Data_Description (Data_Id);  --  , Info_File_Name);
      Put_Line (Routine_Name & "Test_Data_Description completed");
      Test_Features (Data_Id);  --  , Features_File_Name);
      Put_Line (Routine_Name & "Test_Features completed");
      Test_Qualities (Data_Id);  --  , Info_File_Name);

   end Test_Data_Info;

   --  -------------------------------------------------------------------------

   procedure Test_Features (Data_Id : Integer; Use_Files : Boolean := True) is
--     Dataset : String := "") is
      Routine_Name   : constant String := "Test_Features ";
      Feature_Array  : JSON_Array;
      Index          : Positive;
      aFeature       : JSON_Value;
      Feature_Index  : Natural;
   begin
      Put_Line (Routine_Name & "Get features");
      Feature_Array := Get_Data_Features (Data_Id, Use_Files);  --  , File_Name => Dataset);
      Put_Line (Routine_Name & "number of features: " &
                  Integer'Image (Length (Feature_Array)));
      Index := Array_First (Feature_Array);
      while Array_Has_Element (Feature_Array, Index) loop
         aFeature := Array_Element (Feature_Array, Index);
         Feature_Index := Integer'Value (Get (aFeature, "index"));
         if Feature_Index < 4 then
            Put_Line ("aFeature index: " & Integer'Image (Feature_Index));
         end if;
         Index := Array_Next (Feature_Array, Index);
      end loop;

      New_Line;

   end Test_Features;

   --  -------------------------------------------------------------------------

   procedure Test_Fetch_OML is
      Routine_Name      : constant String := "Test_Fetch_OML ";
      Dataset_Name      : constant String := "mnist_784";
--        File_Name         : constant String := "../mnist_784";
--        Feature_File_Name : constant String := "../dataset_554_features";
      Version           : constant String := "1";
      As_Frame          : Unbounded_String := To_Unbounded_String ("false");
      Feature_Array     : JSON_Array;
      Data_Id           : Integer := 0;
      Bunch             : Bunch_Data (True);
   begin
      Put_Line (Routine_Name);
      Feature_Array := Get_Data_Features (Data_Id);  --  , File_Name => File_Name);
      Put_Line (Routine_Name & "Feature_Array set");
      declare
         Target_Column : constant String :=
                           Integer'Image (Length (Feature_Array) + 1);
      begin
         Bunch := Fetch_Openml
           (Dataset_Name  => Dataset_Name, Version => Version,
            Use_Files =>  True, Data_Id => Data_Id,
            Target_Column => Target_Column, Return_X_Y => True,
            As_Frame => As_Frame);
      end;

      Put_Line (Routine_Name & "X length: " &
                  Integer'Image (Length (Bunch.Data)));
      Put_Line (Routine_Name & "Y length: " &
                  Integer'Image (Length (Bunch.Target)));
      Put_Line (Routine_Name & "completed");

   end Test_Fetch_OML;

   --  -------------------------------------------------------------------------

   procedure Test_Qualities (Data_Id : Integer; Use_Files : Boolean := True) is
--     Dataset : String := "") is
      Routine_Name  : constant String := "Test_Qualities ";
      Max_Count     : constant Natural := 10;
      Count         : Natural := 0;
      Quality_Array : Qualities_Map;
      Index         : Positive;
      Quality       : JSON_Value;
   begin
      New_Line;
      Put_Line (Routine_Name);
      Quality_Array := Get_Data_Qualities (Data_Id, Use_Files);  --  , File_Name => Dataset);

      if Is_Empty (Quality_Array) then
         Put_Line (Routine_Name & "there are no qualities");
      else
         Put_Line (Routine_Name & "Quality_Array length: " &
                     Integer'Image (Length (Quality_Array)));
         Index := Array_First (Quality_Array);

         Put_Line (Routine_Name & "first" & Integer'Image (Max_Count) &
                     " qualities:");
         while Array_Has_Element (Quality_Array, Index) loop
            Count := Count + 1;
            Quality := Array_Element (Quality_Array, Index);
            if Count <= Max_Count then
               Put_Line (Routine_Name & "Quality: " & Quality.Write);
            end if;

            Index := Array_Next (Quality_Array, Index);
         end loop;
      end if;

      Put_Line (Routine_Name & "Test_Qualities completed");
      New_Line;

   end Test_Qualities;

   --  -------------------------------------------------------------------------

end OML_File_Tests;
