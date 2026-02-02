# 🎯 SPARC Exercise: Building a Task Management API

## Exercise Overview

Apply the SPARC methodology to build a task management API. This will help you practice the structured approach to development.

## The Task
Build a REST API for managing tasks with the following features:
- Create, read, update, and delete tasks
- Categorize tasks by priority (low, medium, high)
- Filter tasks by status (pending, in_progress, completed)
- Assign tasks to users
- Track task creation and modification timestamps

---

## 📋 Phase 1: Specification (Your Turn!)

**Your Task:**
1. Define user stories for the task management system
2. Specify the API endpoints with HTTP methods and parameters
3. Define data models for tasks and users
4. Identify validation rules
5. Define error handling strategies

**Example Template:**
```
### User Stories:
- As a user, I want to create tasks so I can track my work
- As a user, I want to view all my tasks so I can prioritize my work

### Data Models:
Task:
  - id: integer
  - title: string
  - description: text
  - priority: enum(low, medium, high)
  - status: enum(pending, in_progress, completed)
  - assignee_id: integer (foreign key to users)
  - created_at: timestamp
  - updated_at: timestamp

User:
  - id: integer
  - name: string
  - email: string
```

---

## 🔧 Phase 2: Pseudocode (Your Turn!)

**Your Task:**
Write pseudocode for the following operations:

1. **Create Task:**
```
FUNCTION create_task(title, description, priority, assignee_id):
    # Validate input
    # Check if assignee exists
    # Create task record
    # Return created task
```

2. **Get Tasks by Status:**
```
FUNCTION get_tasks(status):
    # Query database for tasks with given status
    # Return task list
```

3. **Update Task Status:**
```
FUNCTION update_task_status(task_id, new_status):
    # Validate status transition rules
    # Update task record
    # Return updated task
```

4. **Delete Task:**
```
FUNCTION delete_task(task_id):
    # Check if task exists
    # Delete task record
    # Return success response
```

---

## 🏗️ Phase 3: Architecture (Your Turn!)

**Your Task:**
Design the system architecture:

1. **Layer Architecture:**
```
Task Management API
├── API Layer (FastAPI/Flask)
├── Business Logic Layer
├── Data Access Layer
└── Database Layer
```

2. **Database Schema:**
- Define tables and relationships
- Consider indexing for performance

3. **API Design:**
- RESTful endpoint structure
- Request/response models
- Error handling patterns

4. **Security Considerations:**
- Authentication/authorization
- Input validation
- Rate limiting

---

## 💻 Phase 4: Refinement (Your Turn!)

**Your Task:**
Implement the following using TDD principles:

1. **Setup:**
   - Create project structure
   - Install dependencies (FastAPI, SQLAlchemy, pytest)

2. **Write Tests First:**
   ```python
   def test_create_task_valid():
       # Test task creation with valid data
       pass

   def test_create_task_invalid_priority():
       # Test validation for invalid priority
       pass

   def test_update_task_status_transition():
       # Test valid status transitions
       pass

   def test_delete_task():
       # Test task deletion
       pass
   ```

3. **Implementation:**
   - Database models
   - API endpoints
   - Business logic

---

## ✅ Phase 5: Completion (Your Turn!)

**Your Task:**
Complete the project:

1. **Integration Testing:**
   - Test the full CRUD cycle
   - Test error cases
   - Test edge cases

2. **Documentation:**
   - API documentation
   - Setup instructions
   - Usage examples

3. **Quality Assurance:**
   - Run test suite
   - Check code coverage
   - Review for security issues

---

## 🎯 Practice Challenges

Once you complete the basic implementation, try these challenges:

### Challenge 1: Advanced Features
- Add task dependencies (tasks can depend on other tasks)
- Implement task deadlines
- Add task comments
- Create task search functionality

### Challenge 2: Performance
- Add database indexing
- Implement pagination for task lists
- Add caching for frequently accessed tasks
- Optimize database queries

### Challenge 3: Security
- Add JWT authentication
- Implement role-based access control
- Add rate limiting
- Sanitize user inputs

### Challenge 4: DevOps
- Create Docker container
- Add environment configurations
- Set up CI/CD pipeline
- Add logging and monitoring

---

## 📝 Self-Assessment Checklist

Use this checklist to evaluate your implementation:

### Phase 1: Specification
- [ ] All user stories are clearly defined
- [ ] API endpoints are specified completely
- [ ] Data models include all necessary fields
- [ ] Validation rules are comprehensive

### Phase 2: Pseudocode
- [ ] All operations are defined algorithmically
- [ ] Edge cases are considered
- [ ] Error handling is included
- [ ] Logic flows are clear and correct

### Phase 3: Architecture
- [ ] System layers are properly separated
- [ ] Database design is normalized
- [ ] API follows RESTful principles
- [ ] Security considerations are addressed

### Phase 4: Refinement
- [ ] Tests were written before code
- [ ] All test cases pass
- [ ] Code follows best practices
- [ ] Error handling is implemented

### Phase 5: Completion
- [ ] Integration tests are comprehensive
- [ ] Documentation is complete
- [ ] Performance is acceptable
- [ ] Code quality is high

---

## 💡 Tips for Success

1. **Iterate:** Don't try to get everything perfect on the first pass
2. **Test Continuously:** Run tests after every change
3. **Start Small:** Implement basic functionality first, then add features
4. **Document as You Go:** Keep documentation up to date
5. **Review Your Work:** Use peers or code review tools

---

## 🚀 Resources

- **FastAPI Documentation:** https://fastapi.tiangolo.com/
- **SQLAlchemy Tutorial:** https://docs.sqlalchemy.org/en/20/tutorial/
- **Testing with Pytest:** https://docs.pytest.org/
- **REST API Design:** https://restfulapi.net/

Good luck! Remember to follow the SPARC phases systematically for best results.