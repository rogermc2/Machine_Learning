--  Based on scikit-learn/sklearn/datasets _openml.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Dataset_Utilities;
--  with Printing;

with AWS.Client;
with AWS.Response;
with AWS.URL;

with ARFF_Json;

--  pragma Warnings (Off);

package body Openml_Json is

    --     type JSON_Item is record
    --        Name  : Unbounded_String;
    --        Value : Unbounded_String;
    --     end record;

    --     type ARFF_Type is (ARFF_COO, ARFF_DENSE_GEN);

    --     package Tupple_Package is new
    --       Ada.Containers.Vectors (Positive, Unbounded_String);
    --     subtype Tupple_Vector is Tupple_Package.Vector;
    --
    --     use Tupple_Package;
    --     package Zip_Package is new
    --       Ada.Containers.Vectors (Positive, Tupple_Vector);
    --     subtype Zip_Vector is Zip_Package.Vector;

    --     package Pair_Settings_Vector_Package is new
    --       Ada.Containers.Doubly_Linked_Lists (JSON_Item);
    --     subtype Pair_List is Pair_Settings_Vector_Package.List;

    --      package ML_Names_Package is new
    --        Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
    --      subtype Names_List is ML_Names_Package.List;
    --
    --      package ML_Features_Package is new
    --        Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);
    --      subtype Features_Map is ML_Features_Package.Map;

    --     type Json_Data is record
    --        ID                       : Integer := 1;
    --        Name                     : Unbounded_String :=
    --                                     To_Unbounded_String ("anneal");
    --        Version                  : Integer := 2;
    --        Description              : Unbounded_String;
    --        Format                   : String (1 .. 4) := "ARFF";
    --        Upload_Date              : Unbounded_String;
    --        Licence                  : Unbounded_String :=
    --                                     To_Unbounded_String ("Public");
    --        URL                      : Unbounded_String :=
    --                                     To_Unbounded_String ("https:\/\/www.openml.org\/data\/v1\/download\/1\/anneal.arff");
    --        File_id                  : Integer := 1;
    --        Default_Target_Attribute : Unbounded_String := To_Unbounded_String ("class");
    --        Version_Label            : Integer := 2;
    --        Tag                      : Unbounded_String := To_Unbounded_String
    --          ("[study_1, study_41, study_7, uci]");
    --        Visibility               : Unbounded_String :=
    --                                     To_Unbounded_String ("public");
    --        Original_Data_Url        : Unbounded_String :=
    --                                     To_Unbounded_String ("http:\/\/www.openml.org\/d\/2");
    --        Status                   : Unbounded_String :=
    --                                     To_Unbounded_String ("deactivated");
    --        Processing_Date          : Unbounded_String :=
    --                                     To_Unbounded_String ("2018-10-03 21:46:33");
    --        Error                    : Unbounded_String :=
    --                                     To_Unbounded_String ("Read timed out");
    --        Md5_Checksum             : Unbounded_String;
    --     end record;

    Openml_Prefix  : constant String := "http://openml.org/";
    Search_Name    : constant String := "api/v1/json/data/list/data_name/";
    --     Data_Info      : constant String := "api/v1/json/data/";
    Data_Features  : constant String := "api/v1/json/data/features/";
    --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
    --     Data_File      : constant String := "data/v1/download/";

    function Get_Json_Content_From_File (File_Name : String) return JSON_Value;
    function Get_Json_Content_From_Openml_Api (URL : String) return JSON_Value;
    function Get_Num_Samples (Qualities : Qualities_Map) return Integer;
    function Load_Arff_From_File
      (File_Name : String; Return_Type : ARFF_Json.ARFF_Return_Type)
       return JSON_Value;
    procedure Load_Arff_Response (URL : String);
    function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data;
    function Parse_Nominal_Data (Arff_Data       : JSON_Value;
                                 Include_Columns : JSON_Array) return JSON_Array;
    procedure Process_Feature (Dataset_Name  : String;
                               Features_List : JSON_Array);
    procedure Set_Default_Target (Features_List  : JSON_Array;
                                  Target_Columns : out JSON_Array);
    function Split_Sparse_Columns
      (Arff_Data       : ARFF_Json.Arff_Sparse_Data_Type;
       Include_Columns : JSON_Array) return ARFF_Json.Arff_Sparse_Data_Type;
    procedure Verify_Target_Data_Type (Features_Dict  : JSON_Array;
                                       Target_Columns : JSON_Array);

    --  ------------------------------------------------------------------------

    --     function Convert_Arff_Data_Dataframe
    --       (ARFF_Container : ARFF.Arff_Container_Type; Features : JSON_Value)
    --        return JSON_Value is
    --        Routine_Name    : constant String := "Opemml.Convert_Arff_Data_Dataframe";
    --        Description     : constant JSON_Array :=
    --                            Arff_Container.Get ("description");
    --        Relation        : constant String :=
    --                            Arff_Container.Get ("relation");
    --        Attributes      : constant JSON_Array :=
    --                            Arff_Container.Get ("attributes");
    --        ARFF_Data       : constant JSON_Array :=
    --                            Arff_Container.Get ("data");
    --        First_Row       : constant JSON_Value :=
    --                            Array_Element (ARFF_Data, Array_First (ARFF_Data));
    --        Result          : JSON_Value;
    --     begin
    --        return Result;
    --
    --     end Convert_Arff_Data_Dataframe;

    --  ------------------------------------------------------------------------
    --  ArffSparseDataType = Tuple[List, ...]
    --  A Tuple is a collection of Python objects separated by commas.
    --  L325
    procedure Convert_Arff_Data
      (Arff_Container            : JSON_Value;
       Col_Slice_X, Col_Slice_Y  : JSON_Array;
       X, Y                      : out JSON_Array) is
    --        Routine_Name    : constant String := "Opemml.Convert_Arff_Data ";
        ARFF_Data       : constant JSON_Array := Arff_Container.Get ("data");
    begin
        --  L278
        X := Split_Sparse_Columns (ARFF_Data, Col_Slice_X);
        Y := Split_Sparse_Columns (ARFF_Data, Col_Slice_Y);

    end Convert_Arff_Data;

    --  ------------------------------------------------------------------------

    function Download_Data_To_Bunch (URL              : String;
                                     File_Name        : String := "";
                                     Sparse, As_Frame : Boolean;
                                     Features_List    : JSON_Array;
                                     Data_Columns     : JSON_Array;
                                     Target_Columns   : JSON_Array;
                                     Return_X_Y       : Boolean := False)
      --                                       Shape            : Shape_Data)
                                     return Bunch_Data is
        Routine_Name       : constant String := "Openml.Download_Data_To_Bunch ";
        Feature_Index      : Positive := Array_First (Features_List);
        Col_Name           : Positive := Array_First (Features_List);
        Features_Dict      : JSON_Array;
        aFeature           : JSON_Value;
        aColumn            : JSON_Value;
        Feature_Name       : JSON_Value;
        Col_Slice_X        : JSON_Array;
        Col_Slice_Y        : JSON_Array;
        Num_Missing        : Integer;
        Return_Type        : ARFF_Json.ARFF_Return_Type;
        All_Columns        : JSON_Array;
        ARFF_Data          : JSON_Value := Create_Object;
        X                  : JSON_Array;
        Y                  : JSON_Array;
        Nominal_Attributes : JSON_Array;
        --        Frame              : Boolean := False;
        Bunch              : Bunch_Data (Return_X_Y);

        procedure Parse_ARFF
          (ARFF_In          : JSON_Value;
           X_Slice, Y_Slice : JSON_Array;
           X_out, Y_out     : out JSON_Array;
           Nominal_Data_Out : out JSON_Array) is
        begin
            Convert_Arff_Data (ARFF_In, X_Slice, Y_Slice, X_out, Y_out);
            Nominal_Data_Out := Parse_Nominal_Data (ARFF_In, Target_Columns);

        end Parse_ARFF;

        --        procedure
        --        Post_Process (ARFF_Data : JSON_Value; X, Y : out JSON_Array;
        --                      Frame              : Boolean := False;
        --                      Nominal_Attributes : JSON_Array) is
        --        begin
        --           if Frame then
        --              null;
        --           end if;
        --
        --        end Post_Process;

    begin
        Put_Line (Routine_Name);
        Assert (not Is_Empty (Features_List), Routine_Name &
                  "called with empty Features_List.");
        Assert (not Is_Empty (Data_Columns), Routine_Name &
                  "Data_Columns is empty.");
        Assert (Length (Target_Columns) = Length (Data_Columns), Routine_Name &
                  " Target_Columns length" &
                  Integer'Image (Length (Target_Columns)) &
                  " is different to Data_Columns length" &
                  Integer'Image (Length (Data_Columns)));

        while Array_Has_Element (Features_List, Feature_Index) loop
            aFeature := Array_Element (Features_List, Feature_Index);
            Feature_Name := Get (aFeature, "name");
            Append (Features_Dict, Feature_Name);
            Feature_Index := Array_Next (Features_List, Feature_Index);
        end loop;
        Put_Line (Routine_Name & "Features_Dict length" &
                    Integer'Image (Length (Features_Dict)));

        Verify_Target_Data_Type (Features_Dict, Target_Columns);

        --  L566 col_slice_y =
        --        [
        --          int(features_dict[col_name]["index"])
        --          for col_name in target_columns
        --        ]
        Col_Name := Array_First (Features_List);
        while Array_Has_Element (Target_Columns, Col_Name) loop
            aFeature := Array_Element (Features_List, 5);
            aColumn := Get (aFeature, "index");
            --        Put_Line (Routine_Name & "aFeature Y " & aFeature.Write);
            Append (Col_Slice_Y, aColumn);
            Col_Name := Array_Next (Target_Columns, Col_Name);
        end loop;
        Put_Line (Routine_Name & "Col_Slice_Y length" &
                    Integer'Image (Length (Col_Slice_Y)));

        --  L566 continued
        Col_Name := Array_First (Features_List);
        while Array_Has_Element (Data_Columns, Col_Name) loop
            aFeature := Array_Element (Features_List, Col_Name);
            --           Put_Line (Routine_Name & "aFeature X " & aFeature.Write);
            aColumn := Get (aFeature, "index");
            Append (Col_Slice_X, aColumn);
            Col_Name := Array_Next (Data_Columns, Col_Name);
        end loop;

        --  L568
        while Array_Has_Element (Data_Columns, Col_Name) loop
            aFeature := Array_Element (Features_List, Col_Name);
            aColumn := Get (aFeature, "index");
            Append (Col_Slice_X, aColumn);
            Col_Name := Array_Next (Data_Columns, Col_Name);
        end loop;

        --  L569
        Col_Name := Array_First (Col_Slice_Y);
        while Array_Has_Element (Col_Slice_Y, Col_Name) loop
            aFeature := Array_Element (Features_List, Col_Name);
            --           Put_Line (Routine_Name & "aFeature " & aFeature.Write);
            Num_Missing := Integer'Value
              (Get (aFeature, "number_of_missing_values"));
            Assert (Num_Missing >= 0,
                    Routine_Name & "Target column " & " has " & " missing values."
                    & "Missing values are not supported for target columns.");
            Col_Name := Array_Next (Col_Slice_Y, Col_Name);
        end loop;

        --  L582
        if Sparse then
            Return_Type := ARFF_Json.Arff_Coo;
        else
            Return_Type := ARFF_Json.Arff_Dense;
        end if;

        --  L652
        if File_Name'Length > 0 then
            --        if Use_Files then
            --  Load_Arff_Response from file
            ARFF_Data := Load_Arff_From_File
              ("../" & File_Name & ".arff", Return_Type);
            --           Post_Process (ARFF_Data, X, Y, Frame => False,
            --                         Nominal_Attributes =>  Nominal_Attributes);
        else
            Load_Arff_Response (URL);
        end if;
        --        Put_Line (Routine_Name & "L601 ARFF_Data");
        --        Put_Line (ARFF_Data.Write);

        --  L601
        if As_Frame then
            All_Columns := Data_Columns;
            Col_Name := Array_First (Target_Columns);
            while Array_Has_Element (Target_Columns, Col_Name) loop
                Append (All_Columns, Array_Element (Target_Columns, Col_Name));
                Col_Name := Array_Next (Target_Columns, Col_Name);
            end loop;
        end if;

        Put_Line (Routine_Name & "L667");
        --  L667
        Parse_ARFF (ARFF_Data, Col_Slice_X, Col_Slice_Y, X, Y, Nominal_Attributes);
        Put_Line (Routine_Name & "X length" & Integer'Image (Length (X)));
        Put_Line (Routine_Name & "Y length" & Integer'Image (Length (Y)));

        --  L672
        Bunch.Data := X;
        Bunch.Target := Y;
        if not Return_X_Y then
            Bunch.As_Frame := False;
            Bunch.Categories := Nominal_Attributes;
            Bunch.Feature_Names := Data_Columns;
            Bunch.Target_Names := Target_Columns;
        end if;

        return Bunch;

    end Download_Data_To_Bunch;

    --  ------------------------------------------------------------------------

    function Fetch_Openml (Dataset_Name       : String; Version : String := "";
                           Use_Files          : Boolean := True;
                           --                            File_Name          : String := "";
                           --                            Features_File_Name : String := "";
                           Data_Id            : in out Integer;
                           --                            Target_Column      : String := "default-target";
                           Target_Column      : String_List;
                           Return_X_Y         : Boolean := False;
                           As_Frame           : in out Unbounded_String)
                           return Bunch_Data is
        use Ada.Strings;
        use Dataset_Utilities;
        use String_Package;
        Routine_Name    : constant String := "Openml.Fetch_Openml ";
        Dataset_Name_LC : constant String := To_Lower_Case (Dataset_Name);
        JSON_Data_Set   : JSON_Value;
        JSON_Data       : JSON_Value;
        JSON_Data_Info  : JSON_Value;
        JSON_Data_Array : JSON_Array;
        JSON_Data_Item  : JSON_Value;
        Description     : JSON_Value;
        Return_Sparse   : Boolean := False;
        Data_Format     : JSON_Value;
        Data_Status     : JSON_Value;
        Features_List   : JSON_Array;
        Ignore          : JSON_Value;
        Curs            : Cursor;
        Target_Value    : Unbounded_String;
        Target          : constant JSON_Value := Create_Object;
        Target_Columns  : JSON_Array;
        Data_Columns    : JSON_Array;
        --        Shape           : Shape_Data;
        Data_Qualities  : Qualities_Map;
        Bunch           : Bunch_Data (Return_X_Y);
    begin
        --  L862
        JSON_Data_Info := Get_Data_Info_By_Name (Dataset_Name_LC, Version,
                                                 Use_Files => Use_Files);

        JSON_Data := Get (JSON_Data_Info, "data");
        JSON_Data_Set := Get (JSON_Data, "dataset");
        New_Line;
        JSON_Data_Array := Get (JSON_Data_Set);
        JSON_Data_Item := Array_Element (JSON_Data_Array,
                                         Array_First (JSON_Data_Array));
        Data_Id := Get (JSON_Data_Item, "did");
        Put_Line (Routine_Name & "Data_Id: " & Integer'Image (Data_Id));

        --  L877
        Description := Get_Data_Description_By_ID (Data_Id, True);
        Data_Status := Get (Description, "status");
        if To_String (Get (Data_Status)) /= "active" then
            Put_Line (Routine_Name & "Version " &
                        To_String (Get (Get (Description, "version"))) &
                        " of dataset " &
                        To_String (Get (Get (Description, "name"))) &
                        " is inactive meaning that issues have been found in" &
                        " the dataset. Try using a newer version.");
        end if;

        --  L897
        Data_Format := Get (Description, "format");
        --            Put_Line (Routine_Name & Description.Write);
        declare
            Format : String := Get (Data_Format);
        begin
            Format := To_Lower_Case (Format);
            Return_Sparse := Format = "sparse_arff";
        end;

        --  L903
        if As_Frame = "auto" then
            if not Return_Sparse then
                As_Frame := To_Unbounded_String ("true");
            end if;
        else
            As_Frame := To_Unbounded_String ("false");
        end if;

        Assert (not (As_Frame = "true" and Return_Sparse),
                Routine_Name & "cannot return dataframe with sparse data");

        --  L917
        Features_List := Get_Data_Features (Data_ID, True);

        if As_Frame = "false" then
            Process_Feature (Dataset_Name, Features_List);
        end if;

        --  L929
        if Target_Column.Is_Empty then
            Put_Line (Routine_Name & "default-target");
            Set_Default_Target (Features_List, Target_Columns);
        else
            Curs := Target_Column.First;
            while Has_Element (Curs) loop
                Target_Value := Element (Curs);
                Target_Value := To_Unbounded_String
                  (Slice (Target_Value, 2, Length (Target_Value)));
                Trim (Target_Value, Both);
                Set_Field (Target, "target", To_String (Target_Value));
                Append (Target_Columns, Target);
                Next (Curs);
            end loop;
        end if;

        --  L944
        Data_Columns := Valid_Data_Column_Names (Features_List, Target_Columns);

        --  L948
        if not Return_Sparse then
            Data_Qualities := Get_Data_Qualities (Data_Id, True);
            if Get_Num_Samples (Data_Qualities) > - 1 then
                null;
                --              Shape := (Get_Num_Samples (Data_Qualities), Length (Features_List));
            end if;
        end if;

        Put_Line (Routine_Name & "L955 setting bunch");
        --  L955
        if Use_Files then
            Bunch := Download_Data_To_Bunch
              (Dataset_Name, Dataset_Name, False, False, Features_List,
               Data_Columns, Target_Columns);
        else
            Bunch := Download_Data_To_Bunch
              (Dataset_Name, "", False, False, Features_List,
               Data_Columns, Target_Columns);
        end if;
        Put_Line (Routine_Name & "Bunch set");

        --        Bunch.Data := Data_Columns;
        --        Bunch.Target := Target_Columns;
        --        if not Return_X_Y then
        --           Bunch.As_Frame := False;
        --           Bunch.Feature_Names := Data_Columns;
        --           Bunch.Target_Names := Target_Columns;
        --        end if;

        return Bunch;

    end Fetch_Openml;

    --  ------------------------------------------------------------------------

    function Get_Data_Description_By_ID
      (Data_ID : Integer; Use_Files : Boolean := True) return JSON_Value is
        use Ada.Strings;
        Routine_Name : constant String := "Openml.Get_Data_Description_By_ID ";
        URL          : constant String := Data_Features &
                         Fixed.Trim (Integer'Image (Data_ID), Both);
        URL_Object   : AWS.URL.Object;
        Data_Desc    : JSON_Value := Create_Object;
    begin
        if Use_Files then
            declare
                File_Name : constant String := "../dataset_" &
                              Fixed.Trim (Integer'Image (Data_ID), Both) &
                              "_description";
            begin
                Data_Desc := Get_Json_Content_From_File (File_Name);
            end;
        else
            --  URL.Parse parses an URL and returns an Object representing this URL.
            --  It is then possible to extract each part of the URL with other AWS.URL
            --  services.
            URL_Object := AWS.URL.Parse (URL);
            Assert (AWS.URL.Is_Valid (URL_Object), Routine_Name &
                      "object returned by URL " & URL & "is invalid");
            Data_Desc := Get_Json_Content_From_Openml_Api (URL);
        end if;

        --           Put_Line (Routine_Name & "Data_Desc empty? " &
        --                      Boolean'Image (Is_Empty (Data_Desc)));
        if Has_Field (Data_Desc, "data_set_description") then
            --              Put_Line (Routine_Name &
            --                          "Data_Desc has data_set_description field");
            Data_Desc := Get (Data_Desc, "data_set_description");
        else
            Put_Line (Routine_Name & "Data_Desc is not a data_set_description");
        end if;

        return Data_Desc;

    end Get_Data_Description_By_ID;

    --  ------------------------------------------------------------------------

    function Get_Data_Features (Data_ID   : Integer;
                                Use_Files : Boolean := True)
                                return JSON_Array is
        use Ada.Strings;
        Routine_Name  : constant String := "Openml.Get_Data_Features ";
        Json_Data     : JSON_Value := Create_Object;
        Features      : JSON_Value := Create_Object;
        Feature       : JSON_Value := Create_Object;
        Feature_Array : JSON_Array;
    begin
        if Use_Files then
            declare
                File_Name : constant String := "../dataset_" &
                              Fixed.Trim (Integer'Image (Data_ID), Both) &
                              "_features";
            begin
                --              Put_Line (Routine_Name & "File_Name: " & File_Name);
                Json_Data := Get_Json_Content_From_File (File_Name);
            end;
        else
            Json_Data := Get_Json_Content_From_Openml_Api
              (Data_Features & Fixed.Trim (Integer'Image (Data_ID), Both));
        end if;

        Assert (Has_Field (Json_Data, "data_features") or
                  Has_Field (Json_Data, "features"), Routine_Name &
                  "data_features is not a Json_Data field.");

        if Has_Field (Json_Data, "data_features") then
            Features := Get (Json_Data, "data_features");
            Assert (Has_Field (Features, "feature"), Routine_Name &
                      "data_features is not a Json_Data field.");
            Feature := Get (Features, "feature");
            Feature_Array := Get (Feature);
        else
            Feature_Array := Get (Json_Data, "features");
        end if;

        return Feature_Array;

    end Get_Data_Features;

    --  ------------------------------------------------------------------------
    --  L384
    function Get_Data_Info_By_Name (Dataset_Name : String;
                                    Version      : String := "";
                                    Active       : Boolean := False;
                                    Use_Files    : Boolean := True)
      --                                     File_Name         : String := "")
                                    return JSON_Value is
    --        Routine_Name   : constant String := "Openml.Get_Data_Info_By_Name ";
        Openml_Path    : Unbounded_String;
        Json_Data      : JSON_Value;
    begin
        --        if File_Name = "" then
        if not Use_Files then
            Openml_Path := To_Unbounded_String (Search_Name);
            if Active then
                Openml_Path := Openml_Path & "limit/2/status/active/";
            else
                Openml_Path := Openml_Path & Dataset_Name & "/limit/2/" &
                  "data_version/" & Version;
            end if;

            Json_Data := Get_Json_Content_From_Openml_Api
              (To_String (Openml_Path));
        else
            declare
                File_Name : constant String := "../" & Dataset_Name & "_info";
            begin
                Json_Data := Get_Json_Content_From_File (File_Name);
            end;
        end if;

        return Json_Data;

    end Get_Data_Info_By_Name;

    --  ------------------------------------------------------------------------

    function Get_Data_Qualities (Data_ID : Integer; Use_Files : Boolean := True)
      --                                     File_Name : String := "")
                                 return Qualities_Map is
        use Ada.Strings;
        Routine_Name  : constant String := "Openml.Get_Data_Qualities ";
        Json_Data     : JSON_Value;
        Qualities     : JSON_Value;
        Quality_Array : Qualities_Map;

        procedure Get_Quality (Name : Utf8_String; Value : JSON_Value) is
            Quality : constant JSON_Value := Create_Object;
        begin
            Quality.Set_Field (Name, Value);
            Append (Quality_Array, Quality);

        end Get_Quality;

    begin
        --        if File_Name = "" then
        if not Use_Files then
            Json_Data := Get_Json_Content_From_Openml_Api
              (Data_Features & Fixed.Trim (Integer'Image (Data_ID), Both));
        else
            declare
                File_Name : constant String := "../dataset_" &
                              Fixed.Trim (Integer'Image (Data_ID), Both) &
                              "_qualities";
            begin
                Json_Data := Get_Json_Content_From_File (File_Name);
            end;
        end if;

        if Has_Field (Json_Data, "data_qualities") then
            Qualities := Get (Json_Data, "data_qualities");
            Map_JSON_Object (Qualities, Get_Quality'access);
        else
            Put_Line
              (Routine_Name & "Qualities file with" &
                 Integer'Image (Data_ID) &
                 " does not have a data_qualities field.");
        end if;

        return Quality_Array;

    end Get_Data_Qualities;

    --  ------------------------------------------------------------------------

    function Get_Json_Content_From_File (File_Name : String) return JSON_Value is
    --       Routine_Name   : constant String :=
    --                           "Openml.Get_Json_Content_From_File ";
        Name           : constant String := File_Name & ".json";
        File           : File_Type;
        JSON_Data      : Unbounded_String;
        JSON_Main_Node : JSON_Value := Create_Object;
    begin
        Open (File, In_File, Name);
        while not End_Of_File (File) loop
            Append (JSON_Data, To_Unbounded_String (Get_Line (File)));
        end loop;
        Close (File);

        JSON_Main_Node := GNATCOLL.JSON.Read (Strm => JSON_Data, Filename => "");

        return JSON_Main_Node;

    end Get_Json_Content_From_File;

    --  ------------------------------------------------------------------------

    function Get_Json_Content_From_Openml_Api (URL : String)
                                               return JSON_Value is
        Routine_Name      : constant String :=
                              "Openml.Get_Json_Content_From_Openml_Api ";
        AWS_Reply         : constant Aws.Response.Data := Open_Openml_URL (URL);
        AWS_Response      : constant Unbounded_String :=
                              AWS.Response.Message_Body (AWS_Reply);
        JSON_Main_Node    : JSON_Value := Create;
    begin
        Put_Line (Routine_Name & "URL:");
        Put_Line (URL);
        New_Line;
        Put_Line (Routine_Name & "AWS_Response:");
        Put_Line (To_String (AWS_Response));
        JSON_Main_Node := GNATCOLL.JSON.Read
          (Strm => Unbounded_String'(AWS_Response),
           Filename => "");

        return JSON_Main_Node;

    end Get_Json_Content_From_Openml_Api;

    --  ------------------------------------------------------------------------

    function Get_Num_Samples (Qualities : Qualities_Map) return Integer is
    --        Routine_Name  : constant String := "Openml.Get_Num_Samples ";
        Quality       : JSON_Value := Create;
        Index         : Positive := Array_First (Qualities);
        Num_Samples   : Integer := -1;

        procedure Get_Num_Instances (Name : Utf8_String; Value : JSON_Value) is
            Num_Instances : Float := 0.0;
        begin
            if Name = "NumberOfInstances" then
                Num_Instances := Get (Value);
                Num_Samples := Integer (Num_Instances);
            end if;
        end Get_Num_Instances;

        procedure Get_Qual (Name : Utf8_String; Value : JSON_Value) is
            Name_Quality : constant JSON_Value := Create_Object;
        begin
            if Name = "name" and then Kind (Value) = JSON_String_Type then
                declare
                    String_Value : constant String := Get (Value);
                begin
                    if String_Value = "value" then
                        Name_Quality.Set_Field (Name, Value);
                    end if;
                    Map_JSON_Object (Name_Quality, Get_Num_Instances'access);
                end;
            end if;

        end Get_Qual;

    begin
        while Array_Has_Element (Qualities, Index) loop
            Quality := Array_Element (Qualities, Index);
            Map_JSON_Object (Quality, Get_Qual'access);
            Index := Array_Next (Qualities, Index);
        end loop;

        return Num_Samples;

    end Get_Num_Samples;

    --  ------------------------------------------------------------------------

    function Load_Arff_From_File
      (File_Name : String; Return_Type : ARFF_Json.ARFF_Return_Type)
       return JSON_Value is
        File_ID  : File_Type;
        Data     : String_List;
        Count    : Natural := 0;
    begin
        --        Put_Line ("Openml.Load_Arff_From_File");
        Open (File_ID, In_File, File_Name);
        while not End_Of_File (File_ID) loop
            declare
                Text : constant String := Get_Line (File_ID);
            begin
                Count := Count + 1;
                --              Put_Line ("Openml.Load_Arff_From_File line length " &
                --                       Integer'Image (Text'Length));
                Data.Append (To_Unbounded_String (Text));
            end;
            --           Data := Data & To_Unbounded_String (Get_Line (File_ID));
            --           Data := Data & "\r\n";
        end loop;
        Close (File_ID);
        Put_Line ("Openml.Load_Arff_From_File:" & Integer'Image (Count) &
                    " lines loaded");

        return ARFF_Json.Load (Data, Return_Type);

    end Load_Arff_From_File;

    --  ------------------------------------------------------------------------

    procedure Load_Arff_Response (URL : String) is
    --        Response : AWS.Response.Data := Open_Openml_URL (URL);
    begin
        null;

    end Load_Arff_Response;

    --  ------------------------------------------------------------------------

    function Open_Openml_URL (Openml_Path : String) return AWS.Response.Data is
        use AWS;
        Routine_Name   : constant String := "Openml.Open_Openml_URL ";
        URL_Object     : constant AWS.URL.Object :=
                           AWS.URL.Parse (Openml_Prefix & Openml_Path);
        --        Headers        : Client.Header_List;
        AWS_Reply      : Response.Data;
    begin
        Assert (AWS.URL.Is_Valid (URL_Object),
                "Get_Data_Description_By_ID object returned by URL " &
                  Openml_Prefix & Openml_Path & "is invalid");
        --        Headers.Add ("application", "json");
        Put_Line (Routine_Name & "aws url:");
        Put_Line (AWS.URL.URL (URL_Object));
        New_Line;

        AWS_Reply := AWS.Client.Get
          ("http://www.openml.org/api/v1/json/data/list/data_name/mnist_784/limit/2/data_version/1",
           Follow_Redirection => False);
        --  Follow_Redirection => True fails if the redirection is to https which
        --  AWS doesn't support.
        Put_Line (Routine_Name & "data type: " &
                    Response.Content_Type (AWS_Reply));
        New_Line;
        --        JSON_Main_Node := Read
        --          (Strm => Unbounded_String'(Aws.Response.Message_Body(Aws_Reply)),
        --           Filename => "");
        --        Main_Node := Get (JSON_Main_Node);
        --        aValue := Get (Main_Node, 1);
        --        Put_Line (Routine_Name & "data type: " &
        --                    JSON_Value_Type'Image (Kind (aValue)));

        Put_Line  (Routine_Name & "Response ");
        Put_Line  (To_String (AWS.Response.Message_Body (AWS_Reply)));
        Put_Line (Routine_Name & "done");
        return AWS_Reply;

    end Open_Openml_URL;

    --  ------------------------------------------------------------------------

    function Parse_Nominal_Data (Arff_Data       : JSON_Value;
                                 Include_Columns : JSON_Array)
                                 return JSON_Array is
    --        Routine_Name : constant String := "Openml.Parse_Nominal_Data ";
        Attributes   : constant JSON_Array := Get (Arff_Data, "attributes");
        Index_V      : Positive;
        Index_K      : Positive;
        Attribute    : JSON_Value := Create_Object;
        Nominal_Data : JSON_Array;
    begin
        Index_K := Array_First (Include_Columns);
        while Array_Has_Element (Include_Columns, Index_K) loop
            --           Put_Line  (Routine_Name & "Index_K: " & Integer'Image (Index_K));

            Index_V := Array_First (Attributes);
            while Array_Has_Element (Attributes, Index_V) loop
                Attribute := Get (Attributes, Index_V);
                declare
                    Nominal : constant JSON_Value := Attribute;
                begin
                    --                 Put_Line  (Routine_Name & "Nominal: " & Nominal.Write);
                    Append (Nominal_Data, Nominal);
                end;

                Index_V := Array_Next (Attributes, Index_V);
            end loop;

            Index_K := Array_Next (Include_Columns, Index_K);
        end loop;
        New_Line;

        return Nominal_Data;

    end Parse_Nominal_Data;

    --  ------------------------------------------------------------------------

    procedure Process_Feature (Dataset_Name  : String;
                               Features_List : JSON_Array) is
        Routine_Name   : constant String := "Openml.Process_Feature ";
        Feature_Index  : Positive;
        Feature_Name   : JSON_Value;
        Ignore         : JSON_Value;
        Is_Row_ID      : JSON_Value;
        Data_Type_Item : JSON_Value;

        procedure Process_Status (Ignore_Status, Row_ID_Status : JSON_Value) is
        begin
            --  L921
            if not Is_Empty (Ignore_Status) and  not Is_Empty (Row_ID_Status) then
                declare
                    Ignore_Stat : constant String := Get (Ignore_Status);
                    Row_ID_Stat : constant String := Get (Row_ID_Status);
                begin
                    if Ignore_Stat /= "true" and Row_ID_Stat /= "true" then
                        if not Is_Empty (Data_Type_Item) then
                            declare
                                Data_Type : constant String := Get (Data_Type_Item);
                            begin
                                --  923
                                Assert (Data_Type /= "string", Routine_Name & Dataset_Name
                                        & " invalid as STRING attributes are not " &
                                          "supported for array representation. " &
                                          "Try as_frame=True");
                            end;
                        end if;
                    else
                        Data_Type_Item := Get (Feature_Name, "data_type");
                        Put_Line (Routine_Name & "Data_Type_Item set");
                    end if;
                end;
            end if;
        end Process_Status;

    begin
        --  L920
        Feature_Index := Array_First (Features_List);
        while Array_Has_Element (Features_List, Feature_Index) loop
            Feature_Name := Array_Element (Features_List, Feature_Index);
            --           Put_Line (Routine_Name & "Feature_Name JSON type: " &
            --                       JSON_Value_Type'Image (Kind (Feature_Name)));
            Ignore := Get (Feature_Name, "is_ignore");
            Is_Row_ID := Get (Feature_Name, "is_row_identifier");

            Process_Status (Ignore, Is_Row_ID);

            Feature_Index := Array_Next (Features_List, Feature_Index);
        end loop;

    end Process_Feature;

    --  ------------------------------------------------------------------------
    --  L922
    procedure Set_Default_Target (Features_List  : JSON_Array;
                                  Target_Columns : out JSON_Array) is
        Routine_Name  : constant String := "Openml.Set_Default_Target ";
        Feature_Index : Positive;
        Feature       : JSON_Value;
        Target        : JSON_Value;
    begin
        Feature_Index := Array_First (Target_Columns);
        while Array_Has_Element (Features_List, Feature_Index) loop
            Feature := Array_Element (Features_List, Feature_Index);
            if Has_Field (Feature, "is_target") then
                declare
                    Is_Target : constant String := Get (Feature, "is_target");
                begin
                    if Is_Target = "true" then
                        Target := Get (Feature, "name");
                        Append (Target_Columns, Target);
                        Put_Line (Routine_Name & "Target: " & Target.Write);
                    end if;
                end;
            end if;

            Feature_Index := Array_Next (Features_List, Feature_Index);
        end loop;

    end Set_Default_Target;

    --  ------------------------------------------------------------------------
    --  L184  ArffSparseDataType = Tuple[List, ...]
    --  _split_sparse_columns
    --  (arff_data: ArffSparseDataType, include_columns: List)
    --  - > ArffSparseDataType
    --  Arff_Sparse_Data_Type is a subtype of JSON_Array
    function Split_Sparse_Columns
      (Arff_Data       : ARFF_Json.Arff_Sparse_Data_Type;
       Include_Columns : JSON_Array)
       return ARFF_Json.Arff_Sparse_Data_Type is
        use ARFF_Json;
        Routine_Name       : constant String := "Openml.Split_Sparse_Columns ";
        Data_Length        : constant Natural := Length (Arff_Data);
        --        Include_Length     : constant Natural :=
        --                               Natural (Length (Include_Columns));
        Arff_Data_New      : Arff_Sparse_Data_Type;
        New_Row            : JSON_Array;
        Arff_Data_Row      : JSON_Value;
        Arff_Data_Cols     : JSON_Value;
        Columns            : JSON_Array;
        aColumn            : JSON_Value;
        Col                : Positive;
        Include_Col        : Positive;
        Select_Col         : Boolean;
    begin
        Put_Line (Routine_Name & "Data_Length:" & Integer'Image (Data_Length));
        for sample in 1 .. Data_Length loop
            Clear (New_Row);
            Arff_Data_Row := Array_Element (Arff_Data, sample);
            Arff_Data_Cols := Get (Arff_Data_Row, "values");
            Columns := Get (Arff_Data_Cols);
            Col := Array_First (Include_Columns);

            while Array_Has_Element (Include_Columns, Col) loop
                Select_Col := False;
                aColumn := Array_Element (Columns, Col);
                Include_Col := Array_First (Include_Columns);

                while Array_Has_Element (Include_Columns, Include_Col) loop
                    Select_Col := Select_Col or
                      Col = Integer'Value
                        (Get (Get (Include_Columns, Include_Col))) + 1;
                    Include_Col := Array_Next (Include_Columns, Include_Col);
                end loop;

                if Select_Col then
                    Append (New_Row, aColumn);
                end if;
                Col := Array_Next (Include_Columns, Col);
            end loop;
            --              Put_Line (Routine_Name & "end outer while:");

            declare
                New_Data_Row : constant JSON_Value := Create_Object;
            begin
                New_Data_Row.Set_Field ("values", New_Row);
                Append (Arff_Data_New, New_Data_Row);
            end;
        end loop;

        return Arff_Data_New;

    end Split_Sparse_Columns;

    --  ------------------------------------------------------------------------

    function J_Array_To_String_List (J_Array : JSON_Array) return String_List is
        --        Routine_Name  : constant String := "Openml.J_Array_To_String_List ";
        theList       : String_List;
        Index         : Positive := Array_First (J_Array);
        J_Item        : JSON_Value;
        Item          : Unbounded_String;
    begin
        while Array_Has_Element (J_Array, Index) loop
            J_Item := Array_Element (J_Array, Index);
            --           Put_Line (Routine_Name & "J_Item: " & J_Item.Write);
            Item := Get (J_Item);
            theList.Append (Item);
            Index := Array_Next (J_Array, Index);
        end loop;

        return theList;

    end J_Array_To_String_List;

    --  ------------------------------------------------------------------------
    --  L699
    function Valid_Data_Column_Names
      (Features_List, Target_Columns : JSON_Array) return JSON_Array is
    --        Routine_Name  : constant String := "Openml.Valid_Data_Column_Names ";
        Feature_Index : Positive;
        Feature       : JSON_Value;
        Feature_Name  : JSON_Value;
        Ignore        : JSON_Value;
        Is_Row_ID     : JSON_Value;
        Feature_Val   : Unbounded_String;
        Found         : Boolean := False;
        Column_Names  : JSON_Array;

        function Check_Target return Boolean is
            Target_Index : Positive;
            Target       : JSON_Value;
            Target_Found : Boolean := False;
        begin
            --  L707
            --           Put_Line (Routine_Name & "Feature_Val: " & To_String (Feature_Val));
            Target_Index := Array_First (Target_Columns);
            while Array_Has_Element (Target_Columns, Target_Index) and
              not Target_Found loop
                Target := Array_Element (Target_Columns, Target_Index);
                if not Is_Empty (Target) then
                    if Kind (Target) = JSON_Object_Type then
                        declare
                            Target_Val : constant String := Get (Target, "target");
                        begin
                            Target_Found := Target_Val = Feature_Val;
                        end;

                    elsif Kind (Target) = JSON_String_Type then
                        declare
                            Target_String : constant String := Get (Target);
                        begin
                            Target_Found := Target_String = Feature_Val;
                        end;
                    end if;
                end if;

                Target_Index := Array_Next (Target_Columns, Target_Index);
            end loop;

            return Target_Found;

        end Check_Target;

    begin
        --  L705
        Feature_Index := Array_First (Features_List);
        while Array_Has_Element (Features_List, Feature_Index) loop
            Feature := Array_Element (Features_List, Feature_Index);
            Feature_Name := Feature.Get ("name");
            Feature_Val := Get (Feature_Name);
            --           Put_Line (Routine_Name & "Feature_Name: " & Feature_Name.Write);

            Ignore := Feature.Get ("is_ignore");
            Is_Row_ID := Feature.Get ("is_row_identifier");
            --           Put_Line (Routine_Name & "Ignore: " & Ignore.Write);
            --           Put_Line (Routine_Name & "Is_Row_ID: " & Is_Row_ID.Write);
            Found := False;

            if not Is_Empty (Ignore) and not Is_Empty (Is_Row_ID) then
                --                  Put_Line (Routine_Name & "Is_Row_ID and Ignore both not empty");
                declare
                    Ignore_Status : constant String := Get (Ignore);
                    Row_ID_Status : constant String := Get (Is_Row_ID);
                begin
                    if Ignore_Status /= "true" and Row_ID_Status /= "true" then
                        --                          Put_Line (Routine_Name &
                        --                                      "Ignore_Status and Row_ID_Status not true");
                        Found := Check_Target;
                    end if;
                end;

            else  --  Is_Empty (Ignore) or Is_Empty (Is_Row_ID)
                Found := Check_Target;
            end if;

            if not Found then
                Append (Column_Names, Feature_Name);
            end if;

            Feature_Index := Array_Next (Features_List, Feature_Index);
        end loop;

        return Column_Names;

    end Valid_Data_Column_Names;

    --  ------------------------------------------------------------------------

    procedure Verify_Target_Data_Type (Features_Dict  : JSON_Array;
                                       Target_Columns : JSON_Array) is
        Routine_Name  : constant String := "Openml.Verify_Target_Data_Type ";
        Target_Column : Positive := Array_First (Target_Columns);
    begin
        --        Put_Line (Routine_Name & "Target_Columns length" &
        --                    Integer'Image (Length (Target_Columns)));
        while Array_Has_Element (Target_Columns, Target_Column) loop
            Assert (Array_Has_Element (Features_Dict, Target_Column),
                    Routine_Name & "Features_Dict does not have element " &
                      Integer'Image (Target_Column));
            Target_Column := Array_Next (Target_Columns, Target_Column);
        end loop;

    end Verify_Target_Data_Type;

    --  ------------------------------------------------------------------------
    --  The zip() function returns an iterator of tuples where the first item in
    --  each passed iterator is paired together and then the second item in each
    --  passed iterator are paired together etc.
    --  If the passed iterators have different lengths then the iterator with
    --  the least number of items decides the length of the new iterator.

    --     function Zip (List_1, List_2, List_3 : NL_Types.String_Vector)
    --                   return Zip_Vector is
    --        use NL_Types;
    --        Zip_Length : Positive := Positive (List_1.Length);
    --        Tuple      : Tupple_Vector;
    --        Result     : Zip_Vector;
    --     begin
    --        if Positive (List_2.Length) <  Zip_Length then
    --           Zip_Length := Positive (List_2.Length);
    --        end if;
    --        if Positive (List_3.Length) <  Zip_Length then
    --           Zip_Length := Positive (List_3.Length);
    --        end if;
    --
    --        for index in 1 .. Zip_Length loop
    --           Tuple.Clear;
    --           for index in 1 .. 3 loop
    --              Tuple.Append (List_1.Element (index));
    --              Tuple.Append (List_2.Element (index));
    --              Tuple.Append (List_3.Element (index));
    --           end loop;
    --           Result.Append (Tuple);
    --        end loop;
    --
    --        return Result;
    --
    --     end Zip;

    --  ------------------------------------------------------------------------

end Openml_Json;
