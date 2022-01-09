
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_File_Tests is

--      Openml_Prefix  : constant String := "https://openml.org/";
--      Search_Name    : constant String := "api/v1/json/data/list/data_name/";
    --     Data_Info      : constant String := "api/v1/json/data/";
--      Data_Features  : constant String := "api/v1/json/data/features/";
    --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
    --     Data_File      : constant String := "data/v1/download/";

   procedure Test_Data_Info is
      Routine_Name : constant String := "Test_Data_Info ";
      Dataset_Name : constant String := "mnist_784";
      Version      : constant String := "1";
      Data_Info    : JSON_Value;
      JSON_Data_Id : JSON_Value;
      Data_Id      : Integer := 0;
   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version,
                                          From_File => True);
      JSON_Data_Id := Get (Data_Info, "data_id");
      Put_Line (Routine_Name & "JSON_Data_Id kind: " &
                  JSON_Value_Type'Image (JSON_Data_Id.Kind));
      Data_Id := Integer'Value (Get (JSON_Data_Id));
      Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));

   end ;

end OML_File_Tests;
