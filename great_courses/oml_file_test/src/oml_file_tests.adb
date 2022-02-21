
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

with Load_ARFF_Data.ARFF_Printing;
with Openml_Ada; use Openml_Ada;

package body OML_File_Tests is

   --     pragma Warnings (Off);

   --     procedure Test_Features (Data_Id : Integer;  Use_Files : Boolean := True);
   --     Dataset : String := "");
   --     procedure Test_Qualities (Data_Id : Integer; Use_Files : Boolean := True);
   --     Dataset : String := "");

   --  -------------------------------------------------------------------------

   --     procedure Test_Data_Description (Data_Id   : Integer;
   --                                      Use_Files : Boolean := True) is
   --        Routine_Name  : constant String := "Test_Data_Description ";
   --        Description   : JSON_Value;
   --     begin
   --        New_Line;
   --        Description := Get_Data_Description_By_ID (Data_Id, Use_Files);
   --        declare
   --           Desc : constant String := Get (Description, "description");
   --        begin
   --           Put_Line (Routine_Name & "Description length:" &
   --                       Integer'Image (Desc'Length) & " characters.");
   --           --           Put_Line (Routine_Name & "Description:");
   --           --           Put_Line (Desc);
   --        end;
   --        New_Line;
   --
   --     end Test_Data_Description;

   --  -------------------------------------------------------------------------

   --     procedure Test_Data_Info is
   --        Routine_Name    : constant String := "Test_Data_Info ";
   --        Dataset_Name    : constant String := "mnist_784";
   --        Version         : constant String := "1";
   --        Data_Info       : JSON_Value;
   --        Data_Info_Data  : JSON_Value;
   --        Data_Info_Set   : JSON_Value;
   --        Data_Info_Array : JSON_Array;
   --        Data            : JSON_Value;
   --        JSON_Data_Id    : JSON_Value;
   --        Data_Id         : Integer := 0;
   --     begin
   --        New_Line;
   --        Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
   --        Data_Info_Data := Get (Data_Info, "data");
   --        Data_Info_Set := Get (Data_Info_Data, "dataset");
   --        Data_Info_Array := Get (Data_Info_Set);
   --        Data := Array_Element (Data_Info_Array,
   --                               Array_First (Data_Info_Array));
   --        JSON_Data_Id := Get (Data, "did");
   --        Data_Id := Get (JSON_Data_Id);
   --        Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));
   --
   --        Test_Data_Description (Data_Id);
   --        Put_Line (Routine_Name & "Test_Data_Description completed");
   --
   --        Test_Features (Data_Id);  --  , Features_File_Name);
   --        Put_Line (Routine_Name & "Test_Features completed");
   --
   --        Test_Qualities (Data_Id);
   --
   --     end Test_Data_Info;

   --  -------------------------------------------------------------------------

   --     procedure Test_Features (Data_Id : Integer; Use_Files : Boolean := True) is
   --        Routine_Name   : constant String := "Test_Features ";
   --        Feature_Array  : JSON_Array;
   --        Index          : Positive;
   --        aFeature       : JSON_Value;
   --        Feature_Index  : Natural;
   --     begin
   --        Put_Line (Routine_Name & "Get features");
   --        Feature_Array := Get_Data_Features (Data_Id, Use_Files);
   --        Put_Line (Routine_Name & "number of features: " &
   --                    Integer'Image (Length (Feature_Array)));
   --        Index := Array_First (Feature_Array);
   --        while Array_Has_Element (Feature_Array, Index) loop
   --           aFeature := Array_Element (Feature_Array, Index);
   --           Feature_Index := Integer'Value (Get (aFeature, "index"));
   --           if Feature_Index < 4 then
   --              Put_Line ("aFeature index: " & Integer'Image (Feature_Index));
   --           end if;
   --           Index := Array_Next (Feature_Array, Index);
   --        end loop;
   --
   --        New_Line;
   --
   --     end Test_Features;

   --  -------------------------------------------------------------------------

   procedure Test_Fetch_OML is
      use Ada.Containers;
      use Load_ARFF_Data.ARFF_Printing;
      Routine_Name  : constant String := "Test_Fetch_OML ";
      File_Name      : constant String := "../iris.arff";
      Save_File     : constant String := "iris.oml";
--        File_Name     : constant String := "../mnist_784.arff";
--        Save_File     : constant String := "mnist_784.oml";
      As_Frame      : As_Frame_State := As_Frame_False;
      Target_Column : ML_Types.String_List;
      X             : ML_Types.ARFF_Data_List_2D;
      Y             : ML_Types.ARFF_Data_List_2D;
      Bunch         : Bunch_Data;
   begin
      Put_Line (Routine_Name);
      Fetch_Openml (Dataset_File_Name => File_Name,
                    Save_File_Name    => Save_File,
                    Target_Column     => Target_Column,
                    X                 => X,
                    Y                 => Y,
                    Bunch             => Bunch,
                    As_Frame          => As_Frame,
                    Return_X_Y        => True);

      Put_Line (Routine_Name & "X length: " & Count_Type'Image (X.Length));
      Put_Line (Routine_Name & "Y length: " & Count_Type'Image (Y.Length));
      Print_Data (Routine_Name & "X", X, 1, 2);
      Print_Data (Routine_Name & "Y", Y, 1, 2);
      Put_Line (Routine_Name & "completed");

      pragma Unreferenced (Bunch);
      pragma Unreferenced (As_Frame);

   end Test_Fetch_OML;

   --  -------------------------------------------------------------------------

   --     procedure Test_Qualities (Data_Id : Integer; Use_Files : Boolean := True) is
   --        Routine_Name  : constant String := "Test_Qualities ";
   --        Quality_Array : Qualities_Map;
   --        Index         : Positive;
   --        Quality       : JSON_Value;
   --        Quality_Name  : JSON_Value;
   --
   --     begin
   --        New_Line;
   --        Put_Line (Routine_Name);
   --        Quality_Array := Get_Data_Qualities (Data_Id, Use_Files);
   --
   --        if Is_Empty (Quality_Array) then
   --           Put_Line (Routine_Name & "there are no qualities");
   --        else
   --           Put_Line (Routine_Name & "Quality_Array length: " &
   --                       Integer'Image (Length (Quality_Array)));
   --           Index := Array_First (Quality_Array);
   --           while Array_Has_Element (Quality_Array, Index) loop
   --              Quality := Array_Element (Quality_Array, Index);
   --              Quality_Name := Get (Quality, "name");
   --              Put_Line (Routine_Name & "Quality name: " & Quality_Name.Write);
   --              New_Line;
   --              Index := Array_Next (Quality_Array, Index);
   --           end loop;
   --        end if;
   --
   --        Put_Line (Routine_Name & "Test_Qualities completed");
   --        New_Line;
   --
   --     end Test_Qualities;

   --  -------------------------------------------------------------------------

end OML_File_Tests;
