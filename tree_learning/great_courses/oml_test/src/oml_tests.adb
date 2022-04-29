
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Characters.Handling;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Openml; use Openml;

package body OML_Tests is

--      Openml_Prefix  : constant String := "https://openml.org/";
--      Search_Name    : constant String := "api/v1/json/data/list/data_name/";
    --     Data_Info      : constant String := "api/v1/json/data/";
--      Data_Features  : constant String := "api/v1/json/data/features/";
    --     Data_Qualities : constant String := "api/v1/json/data/qualities/";
    --     Data_File      : constant String := "data/v1/download/";

   procedure Test_Data_Info is
      Dataset_Name : constant String := "mnist_784";
      Version      : constant String := "1";
      Data_Info    : JSON_Value;
   begin
      Data_Info := Get_Data_Info_By_Name (Dataset_Name, Version);
   end ;

end OML_Tests;
