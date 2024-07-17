#include "PlanBuilderTests.hpp"

#include <string>
#include <fstream>
#include <iostream>

namespace uxas
{
namespace service
{
namespace test
{

    std::string PlanningState_To_String(uxas::messages::task::PlanningState* state) {
    std::ostringstream oss;
    oss << std::fixed; // Set precision for floating-point numbers
    oss << "PlanningState: EntityID => " << state->getEntityID()
        << ", Latitude => " << state->getPlanningPosition()->getLatitude()
        << ", Longitude => " << state->getPlanningPosition()->getLongitude()
        << ", Altitude => " << state->getPlanningPosition()->getAltitude()
        << ", AltitudeType => " << state->getPlanningPosition()->getAltitudeType()
        << ", Heading => " << state->getPlanningHeading();
    return oss.str();
    }

    std::string PlanningState_Seq_To_String(std::vector<uxas::messages::task::PlanningState*> & states) {
    std::ostringstream oss;
    for (auto element : states) {
        oss << PlanningState_To_String(element);
    }
    return oss.str();
    }

    std::string Int64_Seq_To_String(const std::vector<int64_t> seq) {
    std::ostringstream oss;
    for (auto element : seq) {
        oss << " " + element; // Convert each int64_t to string and append to stream
    }
    return oss.str();
    }

    std::string UniqueAutomationRequest_To_String(std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> request) {
    std::ostringstream oss;
    oss.setf(std::ios_base::boolalpha);
    oss << "UniqueAutomationRequest: RequestID => " << request->getRequestID()
        << ", OperatingRegion => " << request->getOriginalRequest()->getOperatingRegion()
        << ", TaskRelationships => " << request->getOriginalRequest()->getTaskRelationships()
        << ", RedoAllTasks => " << request->getOriginalRequest()->getRedoAllTasks();
    oss << ", Entity List: => " + Int64_Seq_To_String(request->getOriginalRequest()->getEntityList());
    oss << ", Task List: => " + Int64_Seq_To_String(request->getOriginalRequest()->getTaskList());
    oss << PlanningState_Seq_To_String(request->getPlanningStates());
    oss << ", SandboxRequest => " << request->getSandBoxRequest();
    return oss.str();
    }

    void writeStringToFile(const std::string& filename, const std::string& content) {
    // Create an output file stream (ofstream) object
    std::ofstream outFile;

    // Open the file (trunc mode will clear existing content)
    outFile.open(filename, std::ios::out | std::ios::trunc);

    // Check if the file is open
    if (!outFile.is_open()) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return;
    }

    // Write the content to the file
    outFile << content;

    // Close the file
    outFile.close();

    // Inform the user that the content was written successfully
    std::cout << "Content written to file: " << filename << std::endl;
}

    void Write_UniqueAutomationRequest_Map(const std::string& filename, std::unordered_map<int64_t, std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> > map)
    {
        for (const auto& pair : map) {
            writeStringToFile (filename, UniqueAutomationRequest_To_String(pair.second));
        }
    }

    std::string VehicleAction_To_String(afrl::cmasi::VehicleAction* va)
    {
        std::ostringstream oss;
        oss << Int64_Seq_To_String(va->getAssociatedTaskList());
        return oss.str();
    }

  void writeStateToFile(uxas::service::PlanBuilderService service)
    {
    }
    void PlanBuilderTests::Process_Task_Assignment_Summary_Test ()
    {
    auto service = uxas::stduxas::make_unique<uxas::service::PlanBuilderService>();

    std::cerr << "I got here 1" << std::endl;
    std::shared_ptr<uxas::messages::task::TaskAssignmentSummary> taskAssignmentSummary = std::make_shared<uxas::messages::task::TaskAssignmentSummary>();
    taskAssignmentSummary->setCorrespondingAutomationRequestID (1);

    uxas::messages::task::TaskAssignment* task1 = new uxas::messages::task::TaskAssignment();
    uxas::messages::task::TaskAssignment* task2 = new uxas::messages::task::TaskAssignment();

    task1->setTaskID(10LL);
    task2->setTaskID(11LL);

    std::vector<uxas::messages::task::TaskAssignment*>& taskList = taskAssignmentSummary->getTaskList();
    taskList.push_back(task1);
    taskList.push_back(task2);

    std::cerr << "I got here 2" << std::endl;

    std::shared_ptr<afrl::cmasi::EntityState> entityStatePtr = std::make_shared<afrl::cmasi::EntityState>();
    entityStatePtr->setID(100);
    afrl::cmasi::Location3D* location = new afrl::cmasi::Location3D();
    location->setLatitude(1.0);
    location->setLongitude(1.0);
    location->setAltitude(1.0);
    location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    entityStatePtr->setLocation(location);
    entityStatePtr->setHeading(0.0f);
    entityStatePtr->setGroundspeed(54.5f);
    std::vector<int64_t>& tasks = entityStatePtr->getAssociatedTasks();
    tasks.push_back(1);
    tasks.push_back(2);
    entityStatePtr->setTime(350);
    service->m_currentEntityStates[entityStatePtr->getID()] = entityStatePtr;


    std::cerr << "I got here 3" << std::endl;

    std::shared_ptr<afrl::cmasi::EntityState> entityStatePtr2 = std::make_shared<afrl::cmasi::EntityState>();
    entityStatePtr2->setID(101);
    afrl::cmasi::Location3D* _location = new afrl::cmasi::Location3D();
    _location->setLatitude(1.0);
    _location->setLongitude(1.0);
    _location->setAltitude(1.0);
    _location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    entityStatePtr2->setLocation(_location);
    entityStatePtr2->setHeading(0.0f);
    entityStatePtr2->setGroundspeed(54.5f);


    std::cerr << "I got here 4" << std::endl;

    std::vector<int64_t>& tasks2 = entityStatePtr2->getAssociatedTasks();

    std::cerr << "I got here 5" << std::endl;

    tasks2.push_back(1);

    std::cerr << "I got here 6" << std::endl;

    tasks2.push_back(2);
    entityStatePtr2->setTime(350);

    std::cerr << "I got here 7" << std::endl;

    std::cerr << "I got here 8" << std::endl;
    service->m_currentEntityStates[entityStatePtr2->getID()] = entityStatePtr2;


    std::cerr << "I got here 9" << std::endl;

    std::shared_ptr<uxas::messages::task::UniqueAutomationRequest> uniqueAutomationRequest = std::make_shared<uxas::messages::task::UniqueAutomationRequest>();
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()] = uniqueAutomationRequest;
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->setRequestID(taskAssignmentSummary->getCorrespondingAutomationRequestID());
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getOriginalRequest()->getEntityList().push_back(100);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getOriginalRequest()->getEntityList().push_back(101);

    auto planState1 = new uxas::messages::task::PlanningState;
    planState1->setEntityID(100);
    afrl::cmasi::Location3D* location1 = new afrl::cmasi::Location3D();
    location1->setLatitude(1.0);
    location1->setLongitude(1.0);
    location1->setAltitude(1.0);
    location1->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    planState1->setPlanningPosition(location1);
    planState1->setPlanningHeading(0.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState1);

    auto planState2 = new uxas::messages::task::PlanningState;
    planState2->setEntityID(100);
    afrl::cmasi::Location3D* location2 = new afrl::cmasi::Location3D();
    location2->setLatitude(2.0);
    location2->setLongitude(2.0);
    location2->setAltitude(2.0);
    location2->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    planState2->setPlanningPosition(location2);
    planState2->setPlanningHeading(1.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState2);

    auto planState3 = new uxas::messages::task::PlanningState;
    planState3->setEntityID(101);
    afrl::cmasi::Location3D* location3 = new afrl::cmasi::Location3D();
    location3->setLatitude(3.0);
    location3->setLongitude(3.0);
    location3->setAltitude(3.0);
    location3->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    planState3->setPlanningPosition(location3);
    planState3->setPlanningHeading(0.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState3);

    auto planState4 = new uxas::messages::task::PlanningState;
    planState4->setEntityID(102);
    afrl::cmasi::Location3D* location4 = new afrl::cmasi::Location3D();
    location4->setLatitude(4.0);
    location4->setLongitude(4.0);
    location4->setAltitude(4.0);
    location4->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    planState4->setPlanningPosition(location4);
    planState4->setPlanningHeading(1.0);
    service->m_uniqueAutomationRequests[taskAssignmentSummary->getCorrespondingAutomationRequestID()]->getPlanningStates().push_back(planState4);


    std::cerr << "I got here" + std::to_string(taskAssignmentSummary->getCorrespondingAutomationRequestID()) << std::endl;

    service->checkNextTaskImplementationRequest(5);

     std::cerr << "I got here 2" + std::to_string(taskAssignmentSummary->getCorrespondingAutomationRequestID()) << std::endl;


    service->processTaskAssignmentSummary(taskAssignmentSummary);

    Write_UniqueAutomationRequest_Map("Cpp_Process_Task_Assignment_Summary_Test.txt", service->m_uniqueAutomationRequests);

    std::cerr << "I am done" << std::endl;

    // Write State to file
    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test ()
    {
    uxas::service::PlanBuilderService* service = new uxas::service::PlanBuilderService();

    std::shared_ptr<uxas::messages::task::TaskImplementationResponse> taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(100);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();
    Response_In_Progress->setResponseID(1);

    service->m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand();
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand();
    missioncommand1->setVehicleID(100);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;

    service->processTaskImplementationResponse(taskImplementationResponse);
    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test ()
    {
    uxas::service::PlanBuilderService service;

    std::shared_ptr<uxas::messages::task::TaskImplementationResponse> taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(100);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress;
    Response_In_Progress->setResponseID(1);

    service.m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1;
    afrl::cmasi::Waypoint* waypoint2;
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1;
    afrl::cmasi::MissionCommand* missioncommand2;
    missioncommand1->setVehicleID(100);
    missioncommand1->getWaypointList().push_back(waypoint1);
    missioncommand1->getWaypointList().push_back(waypoint2);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service.m_inProgressResponse[1] = Response_In_Progress;

    service.processTaskImplementationResponse(taskImplementationResponse);
    }

    void PlanBuilderTests::Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test ()
    {
    uxas::service::PlanBuilderService service;

    auto taskImplementationResponse = std::make_shared<uxas::messages::task::TaskImplementationResponse>();
    taskImplementationResponse->setResponseID(1);
    taskImplementationResponse->setVehicleID(102);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress;
    Response_In_Progress->setResponseID(1);

    service.m_expectedResponseID[1] = 1;

    afrl::cmasi::Waypoint* waypoint1;
    afrl::cmasi::Waypoint* waypoint2;
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint1);
    taskImplementationResponse->getTaskWaypoints().push_back(waypoint2);

    afrl::cmasi::MissionCommand* missioncommand1;
    afrl::cmasi::MissionCommand* missioncommand2;
    missioncommand1->setVehicleID(100);
    missioncommand1->getWaypointList().push_back(waypoint1);
    missioncommand1->getWaypointList().push_back(waypoint2);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service.m_inProgressResponse[1] = Response_In_Progress;

    service.processTaskImplementationResponse(taskImplementationResponse);
    }

    void PlanBuilderTests::Check_Next_Task_Implementation_Request_Test()
    {
    auto service = uxas::stduxas::make_unique<uxas::service::PlanBuilderService>();

    std::cerr << "I got here 1" << std::endl;

    std::vector< std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> > projectedStates;
    uxas::messages::task::PlanningState* state = new uxas::messages::task::PlanningState;
    afrl::cmasi::Location3D* location = new afrl::cmasi::Location3D();
    location->setLatitude(0.0);
    location->setLongitude(0.0);
    location->setAltitude(0.0);
    location->setAltitudeType(afrl::cmasi::AltitudeType::MSL);
    state->setPlanningPosition(location);
    state->setPlanningHeading(0.0);
    state->setEntityID(1);

    std::cerr << "I got here 2" << std::endl;

    std::shared_ptr<uxas::service::PlanBuilderService::ProjectedState> projectedstate = std::make_shared<uxas::service::PlanBuilderService::ProjectedState>();
    projectedstate->setState(state);
    projectedStates.push_back(projectedstate);

    afrl::cmasi::Waypoint* waypoint1 = new afrl::cmasi::Waypoint();
    afrl::cmasi::Waypoint* waypoint2 = new afrl::cmasi::Waypoint();
    waypoint1->setNumber(1);
    waypoint2->setNumber(2);

    std::shared_ptr<uxas::messages::task::UniqueAutomationResponse> Response_In_Progress = std::make_shared<uxas::messages::task::UniqueAutomationResponse>();;
    std::cerr << "I got here 3" << std::endl;

    afrl::cmasi::MissionCommand* missioncommand1 = new afrl::cmasi::MissionCommand;
    afrl::cmasi::MissionCommand* missioncommand2 = new afrl::cmasi::MissionCommand;
    missioncommand1->setVehicleID(100);
    missioncommand1->getWaypointList().push_back(waypoint1);
    missioncommand1->getWaypointList().push_back(waypoint2);
    missioncommand2->setVehicleID(101);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand1);
    Response_In_Progress->getOriginalResponse()->getMissionCommandList().push_back(missioncommand2);

    service->m_inProgressResponse[1] = Response_In_Progress;
    std::cerr << "I got here 4" << std::endl;

    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_1 = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_1->setVehicleID(1);
    SpeedAltPair_1->setTaskID(0);
    SpeedAltPair_1->setSpeed(0.00000E+00);
    SpeedAltPair_1->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_2 = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_2->setVehicleID(2);
    SpeedAltPair_2->setTaskID(0);
    SpeedAltPair_2->setSpeed(0.00000E+00);
    SpeedAltPair_2->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::list<std::shared_ptr<afrl::impact::SpeedAltPair>> list;
    list.push_back(SpeedAltPair_1);
    list.push_back(SpeedAltPair_2);
    service->m_reqeustIDVsOverrides[1] = list;
    std::cerr << "I got here 5" << std::endl;

    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_3  = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_3->setVehicleID(2);
    SpeedAltPair_3->setTaskID(0);
    SpeedAltPair_3->setSpeed(0.00000E+00);
    SpeedAltPair_3->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::shared_ptr<afrl::impact::SpeedAltPair> SpeedAltPair_4  = std::make_shared<afrl::impact::SpeedAltPair>();
    SpeedAltPair_4->setVehicleID(2);
    SpeedAltPair_4->setTaskID(0);
    SpeedAltPair_4->setSpeed(0.00000E+00);
    SpeedAltPair_4->setAltitudeType(afrl::cmasi::AltitudeType::AGL);
    std::list<std::shared_ptr<afrl::impact::SpeedAltPair>> list2;
    list2.push_back(SpeedAltPair_3);
    list2.push_back(SpeedAltPair_4);
    service->m_reqeustIDVsOverrides[2] = list2;
    std::cerr << "I got here 6" << std::endl;

    service->checkNextTaskImplementationRequest(1);
    std::cerr << "I got here 7" << std::endl;


    // write to file
    }

}; //namespace test
}; //namespace service
}; //namespace uxas
