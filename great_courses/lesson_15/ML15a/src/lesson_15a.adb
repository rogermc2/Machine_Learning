
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Python;
--  with Python_API;

with Support_15A; use Support_15A;

procedure Lesson_15A is
   Program_Name    : constant String := "Lesson 15A ";
   Num_Samples     : constant  Positive := 500;
   Test_Size       : constant  Positive := Positive (0.1 * Float (Num_Samples));
   Train_Size      : constant  Positive := 2 * Test_Size;

   Cats_1 : constant String_9_array (1 .. 36) :=
     ("n01443537", "n01629819", "n01641577", "n01644900", "n01698640", "n01742172",
      "n01855672", "n01882714", "n02002724", "n02056570", "n02058221", "n02074367",
      "n02085620", "n02094433", "n02099601", "n02099712", "n02106662", "n02113799",
      "n02123045", "n02123394", "n02124075", "n02125311", "n02129165", "n02132136",
      "n02364673", "n02395406", "n02403003", "n02410509", "n02415577", "n02423022",
      "n02437312", "n02480495", "n02481823", "n02486410", "n02504458", "n02509815");
   Cats_2  : constant String_9_array (1 .. 14) :=
     ("n01770393", "n01774384", "n01774750", "n01784675", "n02165456", "n02190166",
      "n02206856", "n02226429", "n02231487", "n02233338", "n02236044", "n02268443",
      "n02279972", "n02281406");
   Train_X       :  Image_Vector (1 .. Train_Size);
   Train_Y       :  Integer_Array (1 .. Test_Size);
   Test_X        :  Image_Vector (1 .. Train_Size);
   Test_Y        :  Integer_Array (1 .. Test_Size);

   Classifier       : Python.Module;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_15a");
   Read_Cats (Classifier, Cats_1, 0, Num_Samples, Train_X, Test_X, Train_Y, Test_Y);

   New_Line;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_15A;
