
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
      Description  : JSON_Value;
   begin
      New_Line;
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version,
                                          File_Name => "mnist_784");
      JSON_Data_Id := Get (Data_Info, "data_id");
      Data_Id := Integer'Value (Get (JSON_Data_Id));
      Put_Line (Routine_Name & "Data_Id" & Integer'Image (Data_Id));

      Description := Get_Data_Description_By_ID (Data_Id,
                                                 File_Name => "mnist_784");
      declare
         Desc : String := Get (Description);
      begin
         Put_Line (Routine_Name & "Description length:" &
                     Integer'Image (Desc'Length) & " characters.");
--           Put_Line (Routine_Name & "Description:");
--           Put_Line (Desc);
      end;
      New_Line;

   end Test_Data_Info;

end OML_File_Tests;
