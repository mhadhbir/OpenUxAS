#include "PlanBuilderService.h"

#include "uxas/messages/task/UniqueAutomationRequest.h"
#include "uxas/messages/task/UniqueAutomationResponse.h"
#include "uxas/messages/task/TaskAssignmentSummary.h"
#include "uxas/messages/task/TaskAssignment.h"
#include "uxas/messages/task/TaskImplementationRequest.h"
#include "uxas/messages/task/TaskImplementationResponse.h"
#include "uxas/messages/task/PlanningState.h"

#include "afrl/cmasi/EntityState.h"
#include "afrl/cmasi/GimbalState.h"
#include "afrl/cmasi/GimbalAngleAction.h"
#include "afrl/cmasi/LoiterAction.h"
#include "afrl/impact/SpeedAltPair.h"
#include "afrl/impact/ImpactAutomationRequest.h"
#include "afrl/impact/ImpactAutomationResponse.h"

#include <cstdint> // int64_t
#include <deque>
#include <unordered_map>
#include <list>

namespace uxas
{
namespace service
{
namespace test
{

class PlanBuilderTests
{
    public:
    void Process_Task_Assignment_Summary_Test ();
    void Process_Task_Implementation_Response_Vehicle_Exists_WPList_Empty_Test();
    void Process_Task_Implementation_Response_Vehicle_Exists_WPList_NotEmpty_Test();
    void Process_Task_Implementation_Response_Vehicle_DoesNotExists_Test();
    void Check_Next_Task_Implementation_Request_Test();

};
}; //namespace test
}; //namespace service
}; //namespace uxas
