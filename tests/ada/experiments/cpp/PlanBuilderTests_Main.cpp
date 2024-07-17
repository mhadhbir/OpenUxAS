#include "PlanBuilderTests.hpp"

#include <iostream>

int
main(int argc, char** argv)
{
    std::cerr << "About to allocate tests" << std::endl;
    uxas::service::test::PlanBuilderTests tests;
    std::cerr << "About to call first test" << std::endl;
    tests.Process_Task_Assignment_Summary_Test();
    std::cerr << "Done here" << std::endl;
};
