# 🚀 SPARC Hands-On Tutorial: Building a User Authentication System

## Tutorial Overview

This tutorial will walk you through building a user authentication system using the SPARC methodology. We'll implement a simple but robust authentication system with registration, login, and session management.

## Prerequisites

- Basic understanding of Python
- Knowledge of HTTP concepts
- Familiarity with testing (we'll use pytest)

---

## 📋 Phase 1: Specification

### What is Specification?
Specification is the foundation phase where we define exactly what we need to build. It's like creating a blueprint before construction.

### Activities:
1. Define requirements
2. Identify user stories
3. Define data models
4. Specify API endpoints
5. Define security requirements

### Our Specification:
- **User Registration**: Username (3-20 chars), email, password (min 8 chars, must include numbers and letters)
- **User Login**: Username/email + password
- **Session Management**: JWT tokens with 1-hour expiration
- **Password Security**: Hash passwords using bcrypt
- **API Endpoints**:
  - `POST /register` - User registration
  - `POST /login` - User login
  - `GET /profile` - Get current user profile
  - `POST /logout` - Logout user

### Acceptance Criteria:
- Users can register with valid credentials
- Invalid registration attempts are rejected with clear errors
- Users can login with correct credentials
- Sessions are properly managed
- Passwords are never stored in plaintext
- All endpoints return appropriate HTTP status codes

---

## 🔧 Phase 2: Pseudocode

### What is Pseudocode?
Pseudocode is a detailed algorithmic description of how our system will work. It's implementation-agnostic and focuses on logic flow.

### Registration Logic:
```
FUNCTION register_user(username, email, password):
    # Validate input
    IF username is invalid:
        RETURN error

    IF email is invalid:
        RETURN error

    IF password is invalid:
        RETURN error

    # Check if user already exists
    IF user exists in database:
        RETURN error (user already exists)

    # Hash password
    hashed_password = hash_password(password)

    # Store user
    user = create_user(username, email, hashed_password)

    # Generate JWT token
    token = generate_jwt(user.id)

    # Return success response
    RETURN { user: user, token: token }
```

### Login Logic:
```
FUNCTION login_user(username, password):
    # Find user by username or email
    user = find_user(username)

    # Check if user exists
    IF user is None:
        RETURN error (invalid credentials)

    # Verify password
    IF verify_password(password, user.hashed_password):
        # Generate JWT token
        token = generate_jwt(user.id)

        # Return success response
        RETURN { user: user, token: token }
    ELSE:
        RETURN error (invalid credentials)
```

### Session Validation:
```
FUNCTION get_user_profile(token):
    # Decode JWT token
    payload = decode_jwt(token)

    # Check if token is valid
    IF payload is None:
        RETURN error (invalid token)

    # Check if token is expired
    IF payload.exp < current_time:
        RETURN error (token expired)

    # Get user from database
    user = find_user_by_id(payload.user_id)

    # Return user profile
    RETURN { user: user }
```

---

## 🏗️ Phase 3: Architecture

### What is Architecture?
Architecture phase designs the system structure, components, and interactions. It's like creating the skeleton of our application.

### System Components:

```
Authentication System
├── API Layer (FastAPI)
├── Business Logic Layer
│   ├── User Service
│   ├── Auth Service
│   └── Session Service
├── Data Access Layer
│   ├── User Repository
│   └── Database Models
└── Infrastructure Layer
    ├── Password Hashing
    ├── JWT Library
    └── Database (SQLite for demo)
```

### Data Models:
```python
class User:
    id: int
    username: str
    email: str
    hashed_password: str
    created_at: datetime
    updated_at: datetime

class Session:
    id: int
    user_id: int
    token: str
    expires_at: datetime
```

### API Design:
```python
# Request/Response Models
class RegisterRequest:
    username: str
    email: str
    password: str

class LoginRequest:
    username: str
    password: str

class UserResponse:
    id: int
    username: str
    email: str
    created_at: datetime
```

### Error Handling Strategy:
- 400 Bad Request: Invalid input
- 401 Unauthorized: Authentication failed
- 403 Forbidden: Insufficient permissions
- 404 Not Found: Resource not found
- 500 Internal Server Error: Server error

---

## 💻 Phase 4: Refinement (Implementation)

### What is Refinement?
Refinement is where we implement the system following TDD principles. We write tests first, then implement features to make them pass.

#### Step 1: Set up the project
```bash
mkdir auth_system
cd auth_system
python -m venv venv
source venv/bin/activate  # or venv\Scripts\activate on Windows
pip install fastapi "uvicorn[standard]" python-jose[cryptography] bcrypt passlib
pip install pytest pytest-asyncio pytest-cov httpx
```

#### Step 2: Test-Driven Registration

**Test First (test_auth.py):**
```python
import pytest
from fastapi.testclient import TestClient
from main import app

client = TestClient(app)

def test_register_valid_user():
    """Test registering a user with valid credentials"""
    response = client.post("/register", json={
        "username": "testuser",
        "email": "test@example.com",
        "password": "ValidPass123"
    })
    assert response.status_code == 201
    data = response.json()
    assert "user" in data
    assert "token" in data
    assert data["user"]["username"] == "testuser"
    assert data["user"]["email"] == "test@example.com"

def test_register_duplicate_username():
    """Test registering a duplicate username fails"""
    # First registration
    client.post("/register", json={
        "username": "duplicate",
        "email": "user1@example.com",
        "password": "Pass123"
    })

    # Second registration with same username
    response = client.post("/register", json={
        "username": "duplicate",
        "email": "user2@example.com",
        "password": "Pass123"
    })
    assert response.status_code == 400
    assert "already exists" in response.json()["detail"]

def test_register_invalid_password():
    """Test password validation"""
    response = client.post("/register", json={
        "username": "testuser",
        "email": "test@example.com",
        "password": "weak"
    })
    assert response.status_code == 400
    assert "password" in response.json()["detail"].lower()
```

**Implementation (main.py):**
```python
from fastapi import FastAPI, HTTPException, status
from fastapi.security import HTTPBearer
from pydantic import BaseModel, EmailStr, validator
from datetime import datetime, timedelta
from typing import Optional
import bcrypt
from jose import JWTError, jwt
import sqlite3
from contextlib import contextmanager

app = FastAPI()
security = HTTPBearer()

DATABASE = "auth.db"
SECRET_KEY = "your-secret-key-here"
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 60

# Database setup
def init_db():
    conn = sqlite3.connect(DATABASE)
    cursor = conn.cursor()
    cursor.execute('''
        CREATE TABLE IF NOT EXISTS users (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            username TEXT UNIQUE NOT NULL,
            email TEXT UNIQUE NOT NULL,
            hashed_password TEXT NOT NULL,
            created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        )
    ''')
    conn.commit()
    conn.close()

# Models
class RegisterRequest(BaseModel):
    username: str
    email: EmailStr
    password: str

    @validator('username')
    def validate_username(cls, v):
        if len(v) < 3 or len(v) > 20:
            raise ValueError('Username must be 3-20 characters')
        return v

    @validator('password')
    def validate_password(cls, v):
        if len(v) < 8:
            raise ValueError('Password must be at least 8 characters')
        if not any(c.isdigit() for c in v):
            raise ValueError('Password must contain at least one digit')
        if not any(c.isalpha() for c in v):
            raise ValueError('Password must contain at least one letter')
        return v

class UserResponse(BaseModel):
    id: int
    username: str
    email: str
    created_at: datetime

# Database operations
@contextmanager
def get_db():
    conn = sqlite3.connect(DATABASE)
    try:
        yield conn
    finally:
        conn.close()

def hash_password(password: str) -> str:
    return bcrypt.hashpw(password.encode(), bcrypt.gensalt()).decode()

def verify_password(password: str, hashed: str) -> bool:
    return bcrypt.checkpw(password.encode(), hashed.encode())

def create_user(username: str, email: str, password: str) -> UserResponse:
    with get_db() as conn:
        cursor = conn.cursor()
        cursor.execute(
            "INSERT INTO users (username, email, hashed_password) VALUES (?, ?, ?)",
            (username, email, hash_password(password))
        )
        user_id = cursor.lastrowid
        cursor.execute("SELECT * FROM users WHERE id = ?", (user_id,))
        row = cursor.fetchone()
        return UserResponse(
            id=row[0],
            username=row[1],
            email=row[2],
            created_at=datetime.fromisoformat(row[4])
        )

def get_user_by_username(username: str) -> Optional[dict]:
    with get_db() as conn:
        cursor = conn.cursor()
        cursor.execute("SELECT * FROM users WHERE username = ?", (username,))
        row = cursor.fetchone()
        return dict(row) if row else None

# Endpoints
@app.post("/register")
def register(request: RegisterRequest):
    # Check if user already exists
    if get_user_by_username(request.username):
        raise HTTPException(
            status_code=400,
            detail="Username already exists"
        )

    # Create user
    user = create_user(request.username, request.email, request.password)

    # Generate JWT token
    access_token = create_access_token(data={"sub": user.username})

    return {
        "user": user,
        "token": access_token
    }

# Token creation
def create_access_token(data: dict, expires_delta: Optional[timedelta] = None):
    to_encode = data.copy()
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=15)
    to_encode.update({"exp": expire})
    encoded_jwt = jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)
    return encoded_jwt

if __name__ == "__main__":
    init_db()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

#### Step 3: Continue with login functionality

**Tests:**
```python
def test_login_valid_user():
    """Test login with valid credentials"""
    # Register first
    client.post("/register", json={
        "username": "loginuser",
        "email": "login@example.com",
        "password": "LoginPass123"
    })

    # Login
    response = client.post("/login", json={
        "username": "loginuser",
        "password": "LoginPass123"
    })
    assert response.status_code == 200
    data = response.json()
    assert "user" in data
    assert "token" in data

def test_login_invalid_credentials():
    """Test login with invalid credentials"""
    response = client.post("/login", json={
        "username": "nonexistent",
        "password": "wrongpass"
    })
    assert response.status_code == 401
```

**Implementation:**
```python
class LoginRequest(BaseModel):
    username: str
    password: str

@app.post("/login")
def login(request: LoginRequest):
    # Find user
    user = get_user_by_username(request.username)
    if not user or not verify_password(request.password, user['hashed_password']):
        raise HTTPException(
            status_code=401,
            detail="Invalid credentials"
        )

    # Generate token
    access_token = create_access_token(data={"sub": user['username']})

    return {
        "user": UserResponse(
            id=user['id'],
            username=user['username'],
            email=user['email'],
            created_at=datetime.fromisoformat(user['created_at'])
        ),
        "token": access_token
    }
```

---

## ✅ Phase 5: Completion

### What is Completion?
Completion phase focuses on integration, testing, and ensuring the system meets all requirements.

#### Step 1: Integration Testing
```python
def test_full_auth_flow():
    """Test complete registration → login → profile → logout flow"""
    # Register
    reg_response = client.post("/register", json={
        "username": "flowuser",
        "email": "flow@example.com",
        "password": "FlowPass123"
    })
    token = reg_response.json()["token"]

    # Get profile
    headers = {"Authorization": f"Bearer {token}"}
    profile_response = client.get("/profile", headers=headers)
    assert profile_response.status_code == 200
    assert profile_response.json()["user"]["username"] == "flowuser"

    # Logout (simulated by token invalidation)
    # In real implementation, you'd have a logout endpoint
    # that adds the token to a blocklist
```

#### Step 2: Performance Considerations
```python
# Add rate limiting to prevent brute force attacks
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware

app.add_middleware(TrustedHostMiddleware, allowed_hosts=["example.com", "*.example.com"])
app.add_middleware(CORSMiddleware, allow_origins=["*"])
```

#### Step 3: Documentation
```markdown
# User Authentication API

## Overview
This API provides user authentication capabilities including registration, login, and session management.

## Endpoints

### Register User
- POST /register
- Request: { username, email, password }
- Response: { user, token }

### Login
- POST /login
- Request: { username, password }
- Response: { user, token }

### Get Profile
- GET /profile
- Headers: Authorization: Bearer <token>
- Response: { user }

### Security Features
- Password hashing with bcrypt
- JWT token authentication
- Input validation
- Rate limiting (recommended)
- CORS protection
```

---

## 🎯 Key Takeaways

### SPARC Benefits:
1. **Clear Requirements**: Specification prevents scope creep
2. **Structured Design**: Architecture ensures maintainability
3. **Test-First**: Refinement produces robust code
4. **Complete Coverage**: Completion ensures quality

### Common Patterns:
- Write tests before code (TDD)
- Separate concerns (layers)
- Validate all inputs
- Handle errors gracefully
- Document everything

### Anti-Patterns to Avoid:
- Skipping specification leads to unclear requirements
- No tests leads to bugs
- Mixing concerns leads to unmaintainable code
- No error handling leads to security issues

---

## 🚀 Next Steps

1. **Practice**: Try implementing additional features like password reset
2. **Enhance**: Add OAuth integration, two-factor authentication
3. **Scale**: Add database migrations, environment configurations
4. **Deploy**: Containerize with Docker, deploy to cloud

Remember: SPARC is iterative! As you discover new requirements, loop back to earlier phases to refine your system.