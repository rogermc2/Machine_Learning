
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Tests; use Classifier_Tests;
--  with Decision_Path_Tests; use Decision_Path_Tests;
--  with Split_Tests; use Split_Tests;

procedure Test_Classifier is

begin
   Put_Line ("Test Classifier started");
   Test_Classification_Toy;
--     Test_Probability;
--     Put_Line ("Test Iris started");
--     Test_Iris;
--     Put_Line ("Test Decision Path started");
--     Test_Decision_Path;
--     Put_Line ("Test Min Samples Split started");
--     Test_Min_Samples_Split;

end Test_Classifier;
