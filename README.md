# Enabledao - Disability Aid DAO

A decentralized autonomous organization (DAO) for autonomous disability aid distribution and community voting on the Stacks blockchain.

## Overview

Enabledao enables communities to collectively fund and distribute aid to individuals with disabilities through a transparent, democratic voting process. Members can propose aid requests, vote on proposals, and track distribution of funds.

## Features

- **Member Management**: Join DAO and get verified status
- **Proposal System**: Create aid requests with descriptions and amounts
- **Democratic Voting**: Weighted voting system (verified members get 2x weight)
- **Treasury Management**: Community-funded treasury with transparent distribution
- **Aid Tracking**: Track aid received by individuals
- **Governance**: Configurable voting periods and quorum requirements

## Core Functions

### Member Operations

#### `join-dao`
Join the DAO as a new member.
```clarity
(contract-call? .enabledao join-dao)
```

#### `verify-member`
Verify a member (admin only).
```clarity
(contract-call? .enabledao verify-member 'SP1EXAMPLE...)
```

### Proposal Operations

#### `create-proposal`
Create a new aid proposal (verified members only).
```clarity
(contract-call? .enabledao create-proposal 
  "Medical Equipment" 
  "Funding for wheelchair accessibility modifications" 
  u1000000 
  'SP1RECIPIENT...)
```

#### `vote-on-proposal`
Vote on an active proposal.
```clarity
(contract-call? .enabledao vote-on-proposal u1 true)
```

#### `execute-proposal`
Execute a passed proposal to distribute funds.
```clarity
(contract-call? .enabledao execute-proposal u1)
```

### Treasury Operations

#### `fund-treasury`
Add funds to the DAO treasury.
```clarity
(contract-call? .enabledao fund-treasury u5000000)
```

## Read-Only Functions

### `get-member-info`
Get member information.
```clarity
(contract-call? .enabledao get-member-info 'SP1EXAMPLE...)
```

### `get-proposal-info`
Get proposal details.
```clarity
(contract-call? .enabledao get-proposal-info u1)
```

### `get-treasury-balance`
Get current treasury balance.
```clarity
(contract-call? .enabledao get-treasury-balance)
```

### `is-proposal-active`
Check if a proposal is currently active for voting.
```clarity
(contract-call? .enabledao is-proposal-active u1)
```

### `can-execute-proposal`
Check if a proposal can be executed.
```clarity
(contract-call? .enabledao can-execute-proposal u1)
```

## Voting System

- **Regular Members**: 1 vote per proposal
- **Verified Members**: 2 votes per proposal (verified by admin)
- **Quorum**: Minimum 3 votes required for proposal execution
- **Voting Period**: Default 144 blocks (~24 hours)
- **Execution**: Proposals pass with simple majority after voting period ends

## Treasury Management

- Community members fund the treasury through `fund-treasury`
- Funds are distributed automatically when proposals are executed
- Emergency withdrawal available to contract owner
- All transactions are transparent and tracked on-chain

## Admin Functions

- `verify-member`: Grant verification status to members
- `update-voting-period`: Modify voting duration
- `update-quorum`: Change minimum vote threshold
- `emergency-withdraw`: Emergency treasury access

## Error Codes

- `u100`: Unauthorized access
- `u101`: Invalid proposal
- `u102`: Already voted
- `u103`: Proposal expired
- `u104`: Proposal not executable
- `u105`: Insufficient funds
- `u106`: Member not found
- `u107`: Already a member
- `u108`: Invalid amount
- `u109`: Proposal not found

## Development

### Prerequisites
- Clarinet CLI
- Stacks blockchain access

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy
```

## Usage Example

1. **Join the DAO**
   ```clarity
   (contract-call? .enabledao join-dao)
   ```

2. **Fund the treasury**
   ```clarity
   (contract-call? .enabledao fund-treasury u10000000)
   ```

3. **Create aid proposal** (after verification)
   ```clarity
   (contract-call? .enabledao create-proposal 
     "Assistive Technology" 
     "Screen reader software for visually impaired student" 
     u500000 
     'SP1STUDENT...)
   ```

4. **Vote on proposal**
   ```clarity
   (contract-call? .enabledao vote-on-proposal u1 true)
   ```

5. **Execute proposal** (after voting period)
   ```clarity
   (contract-call? .enabledao execute-proposal u1)
   ```

## Security Features

- Admin-only verification system
- Voting period enforcement
- Quorum requirements
- Double-voting prevention
- Treasury balance validation
- Emergency controls

## License

MIT License
