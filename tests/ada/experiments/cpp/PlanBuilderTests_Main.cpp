#include "PlanBuilderTests.hpp"

int
main(int argc, char** argv)
{
    uxas::service::test::PlanBuilderTests tests;
    tests.Process_Task_Assignment_Summary_Test();
    tests.Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test();
};