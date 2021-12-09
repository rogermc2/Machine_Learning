
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Tests; use Classifier_Tests;
--  with Decision_Path_Tests; use Decision_Path_Tests;
--  with Split_Tests; use Split_Tests;

procedure Test_Classifier is

begin
   Put_Line ("Test Classifier started");
   Test_Classification_Toy;
   New_Line;
   Put_Line ("Test Probability started");
   Test_Probability;
   New_Line;
--     Put_Line ("Test Iris started");
--     Test_Iris;
--     New_Line;
--     Put_Line ("Test Decision Path started");
--     Test_Decision_Path;
--     New_Line;
--     Put_Line ("Test Min Samples Split started");
--     Test_Min_Samples_Split;

end Test_Classifier;
