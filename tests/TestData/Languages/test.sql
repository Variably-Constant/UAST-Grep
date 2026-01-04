-- SQL Test File for UAST-Grep
-- Tests: DDL, DML, queries, functions, procedures, transactions

-- =============================================================================
-- Database and Schema
-- =============================================================================

-- Create database
CREATE DATABASE IF NOT EXISTS uast_grep_test
    CHARACTER SET utf8mb4
    COLLATE utf8mb4_unicode_ci;

USE uast_grep_test;

-- Create schema
CREATE SCHEMA IF NOT EXISTS test_schema;

-- =============================================================================
-- Table Definitions (DDL)
-- =============================================================================

-- Drop table if exists
DROP TABLE IF EXISTS users CASCADE;

-- Create table with various column types
CREATE TABLE users (
    id              SERIAL PRIMARY KEY,
    uuid            UUID DEFAULT gen_random_uuid() NOT NULL,
    username        VARCHAR(50) NOT NULL UNIQUE,
    email           VARCHAR(255) NOT NULL,
    password_hash   CHAR(64) NOT NULL,
    first_name      VARCHAR(100),
    last_name       VARCHAR(100),
    birth_date      DATE,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at      TIMESTAMP WITH TIME ZONE,
    last_login      TIMESTAMP,
    is_active       BOOLEAN DEFAULT TRUE,
    role            VARCHAR(20) DEFAULT 'user' CHECK (role IN ('admin', 'user', 'guest')),
    balance         DECIMAL(10, 2) DEFAULT 0.00,
    metadata        JSONB,

    -- Table constraint
    CONSTRAINT email_format CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$')
);

-- Create related table with foreign key
CREATE TABLE posts (
    id              SERIAL PRIMARY KEY,
    user_id         INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    title           VARCHAR(255) NOT NULL,
    slug            VARCHAR(255) NOT NULL UNIQUE,
    content         TEXT,
    excerpt         TEXT,
    status          VARCHAR(20) DEFAULT 'draft',
    view_count      INTEGER DEFAULT 0,
    published_at    TIMESTAMP WITH TIME ZONE,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at      TIMESTAMP WITH TIME ZONE,

    CONSTRAINT status_values CHECK (status IN ('draft', 'published', 'archived'))
);

-- Create junction table for many-to-many
CREATE TABLE post_tags (
    post_id         INTEGER NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
    tag_id          INTEGER NOT NULL REFERENCES tags(id) ON DELETE CASCADE,
    created_at      TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

    PRIMARY KEY (post_id, tag_id)
);

CREATE TABLE tags (
    id              SERIAL PRIMARY KEY,
    name            VARCHAR(50) NOT NULL UNIQUE,
    slug            VARCHAR(50) NOT NULL UNIQUE,
    description     TEXT
);

-- Create table with composite primary key
CREATE TABLE order_items (
    order_id        INTEGER NOT NULL,
    product_id      INTEGER NOT NULL,
    quantity        INTEGER NOT NULL DEFAULT 1 CHECK (quantity > 0),
    unit_price      DECIMAL(10, 2) NOT NULL,
    discount        DECIMAL(5, 2) DEFAULT 0.00,

    PRIMARY KEY (order_id, product_id)
);

-- =============================================================================
-- Indexes
-- =============================================================================

-- Simple index
CREATE INDEX idx_users_email ON users(email);

-- Unique index
CREATE UNIQUE INDEX idx_users_username ON users(username);

-- Composite index
CREATE INDEX idx_posts_user_status ON posts(user_id, status);

-- Partial index
CREATE INDEX idx_active_users ON users(username) WHERE is_active = TRUE;

-- Expression index
CREATE INDEX idx_users_lower_email ON users(LOWER(email));

-- GIN index for JSONB
CREATE INDEX idx_users_metadata ON users USING GIN(metadata);

-- Full-text search index
CREATE INDEX idx_posts_content_fts ON posts USING GIN(to_tsvector('english', content));

-- =============================================================================
-- Views
-- =============================================================================

-- Simple view
CREATE OR REPLACE VIEW active_users AS
SELECT id, username, email, first_name, last_name, created_at
FROM users
WHERE is_active = TRUE;

-- View with joins
CREATE OR REPLACE VIEW post_details AS
SELECT
    p.id,
    p.title,
    p.slug,
    p.excerpt,
    p.status,
    p.view_count,
    p.published_at,
    u.username AS author_name,
    u.email AS author_email,
    ARRAY_AGG(t.name) AS tags
FROM posts p
INNER JOIN users u ON p.user_id = u.id
LEFT JOIN post_tags pt ON p.id = pt.post_id
LEFT JOIN tags t ON pt.tag_id = t.id
GROUP BY p.id, u.username, u.email;

-- Materialized view
CREATE MATERIALIZED VIEW user_stats AS
SELECT
    u.id,
    u.username,
    COUNT(p.id) AS post_count,
    COALESCE(SUM(p.view_count), 0) AS total_views,
    MAX(p.published_at) AS last_post_date
FROM users u
LEFT JOIN posts p ON u.id = p.user_id AND p.status = 'published'
GROUP BY u.id, u.username;

-- =============================================================================
-- Data Manipulation (DML)
-- =============================================================================

-- Insert single row
INSERT INTO users (username, email, password_hash, first_name, last_name)
VALUES ('johndoe', 'john@example.com', 'hash123', 'John', 'Doe');

-- Insert multiple rows
INSERT INTO tags (name, slug, description) VALUES
    ('technology', 'technology', 'Technology related posts'),
    ('programming', 'programming', 'Programming tutorials'),
    ('database', 'database', 'Database topics');

-- Insert with returning
INSERT INTO posts (user_id, title, slug, content, status)
VALUES (1, 'First Post', 'first-post', 'Content here', 'published')
RETURNING id, created_at;

-- Insert from select
INSERT INTO user_stats (id, username, post_count)
SELECT id, username, 0 FROM users WHERE is_active = TRUE;

-- Update single row
UPDATE users
SET last_login = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP
WHERE id = 1;

-- Update with join (PostgreSQL syntax)
UPDATE posts p
SET view_count = view_count + 1
FROM users u
WHERE p.user_id = u.id AND u.username = 'johndoe';

-- Update with case
UPDATE users
SET role = CASE
    WHEN balance > 1000 THEN 'premium'
    WHEN balance > 100 THEN 'standard'
    ELSE 'basic'
END
WHERE is_active = TRUE;

-- Delete with condition
DELETE FROM posts WHERE status = 'draft' AND created_at < NOW() - INTERVAL '30 days';

-- Upsert (INSERT ON CONFLICT)
INSERT INTO users (username, email, password_hash)
VALUES ('newuser', 'new@example.com', 'hash456')
ON CONFLICT (username) DO UPDATE
SET email = EXCLUDED.email, updated_at = CURRENT_TIMESTAMP;

-- =============================================================================
-- Complex Queries
-- =============================================================================

-- Select with multiple clauses
SELECT
    u.id,
    u.username,
    u.email,
    COUNT(p.id) AS post_count,
    MAX(p.published_at) AS latest_post
FROM users u
LEFT JOIN posts p ON u.id = p.user_id AND p.status = 'published'
WHERE u.is_active = TRUE
    AND u.created_at >= '2024-01-01'
GROUP BY u.id, u.username, u.email
HAVING COUNT(p.id) > 0
ORDER BY post_count DESC, u.username ASC
LIMIT 10
OFFSET 0;

-- Subquery
SELECT * FROM users
WHERE id IN (
    SELECT DISTINCT user_id FROM posts WHERE status = 'published'
);

-- CTE (Common Table Expression)
WITH active_authors AS (
    SELECT DISTINCT user_id
    FROM posts
    WHERE status = 'published' AND published_at >= NOW() - INTERVAL '30 days'
),
author_stats AS (
    SELECT
        u.id,
        u.username,
        COUNT(p.id) AS recent_posts
    FROM users u
    INNER JOIN active_authors aa ON u.id = aa.user_id
    LEFT JOIN posts p ON u.id = p.user_id
    GROUP BY u.id, u.username
)
SELECT * FROM author_stats ORDER BY recent_posts DESC;

-- Recursive CTE
WITH RECURSIVE category_tree AS (
    SELECT id, name, parent_id, 1 AS level
    FROM categories
    WHERE parent_id IS NULL
    UNION ALL
    SELECT c.id, c.name, c.parent_id, ct.level + 1
    FROM categories c
    INNER JOIN category_tree ct ON c.parent_id = ct.id
)
SELECT * FROM category_tree ORDER BY level, name;

-- Window functions
SELECT
    id,
    title,
    user_id,
    view_count,
    ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY view_count DESC) AS rank,
    SUM(view_count) OVER (PARTITION BY user_id) AS total_user_views,
    AVG(view_count) OVER () AS avg_views,
    LAG(view_count) OVER (ORDER BY published_at) AS prev_views,
    LEAD(view_count) OVER (ORDER BY published_at) AS next_views
FROM posts
WHERE status = 'published';

-- UNION and set operations
SELECT username, email, 'user' AS source FROM users WHERE is_active = TRUE
UNION ALL
SELECT name, email, 'admin' AS source FROM admins
EXCEPT
SELECT username, email, 'user' FROM banned_users;

-- =============================================================================
-- Functions and Procedures
-- =============================================================================

-- Create function
CREATE OR REPLACE FUNCTION get_user_post_count(user_id_param INTEGER)
RETURNS INTEGER AS $$
DECLARE
    post_count INTEGER;
BEGIN
    SELECT COUNT(*) INTO post_count
    FROM posts
    WHERE user_id = user_id_param AND status = 'published';

    RETURN post_count;
END;
$$ LANGUAGE plpgsql;

-- Function with multiple parameters
CREATE OR REPLACE FUNCTION search_posts(
    search_term TEXT,
    status_filter VARCHAR(20) DEFAULT NULL,
    limit_count INTEGER DEFAULT 10
)
RETURNS TABLE (
    id INTEGER,
    title VARCHAR(255),
    excerpt TEXT,
    published_at TIMESTAMP WITH TIME ZONE
) AS $$
BEGIN
    RETURN QUERY
    SELECT p.id, p.title, p.excerpt, p.published_at
    FROM posts p
    WHERE (search_term IS NULL OR p.content ILIKE '%' || search_term || '%')
        AND (status_filter IS NULL OR p.status = status_filter)
    ORDER BY p.published_at DESC
    LIMIT limit_count;
END;
$$ LANGUAGE plpgsql;

-- Stored procedure
CREATE OR REPLACE PROCEDURE update_user_stats()
LANGUAGE plpgsql
AS $$
BEGIN
    REFRESH MATERIALIZED VIEW user_stats;
    RAISE NOTICE 'User stats updated at %', NOW();
END;
$$;

-- Trigger function
CREATE OR REPLACE FUNCTION update_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger
CREATE TRIGGER users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at();

-- =============================================================================
-- Transactions
-- =============================================================================

BEGIN;

    INSERT INTO users (username, email, password_hash)
    VALUES ('transaction_test', 'trans@test.com', 'hash');

    SAVEPOINT user_created;

    INSERT INTO posts (user_id, title, slug, content)
    VALUES (LASTVAL(), 'Test Post', 'test-post', 'Content');

    -- Rollback to savepoint if needed
    -- ROLLBACK TO SAVEPOINT user_created;

COMMIT;

-- =============================================================================
-- Grants and Security
-- =============================================================================

-- Create role
CREATE ROLE app_user WITH LOGIN PASSWORD 'secure_password';

-- Grant privileges
GRANT SELECT, INSERT, UPDATE ON users TO app_user;
GRANT SELECT ON posts TO app_user;
GRANT USAGE, SELECT ON SEQUENCE users_id_seq TO app_user;

-- Revoke privileges
REVOKE DELETE ON users FROM app_user;

-- Row-level security
ALTER TABLE posts ENABLE ROW LEVEL SECURITY;

CREATE POLICY posts_owner_policy ON posts
    FOR ALL
    USING (user_id = current_user_id());
