// Cairo Test File for UAST-Grep
// Tests: contracts, functions, storage, events, traits

use starknet::ContractAddress;
use starknet::get_caller_address;
use starknet::get_block_timestamp;

// Constants
const MAX_ITEMS: u256 = 100;
const DEFAULT_FEE: u256 = 1000;

// Errors
mod Errors {
    const UNAUTHORIZED: felt252 = 'Caller is not authorized';
    const INVALID_AMOUNT: felt252 = 'Invalid amount provided';
    const INSUFFICIENT_BALANCE: felt252 = 'Insufficient balance';
    const ALREADY_INITIALIZED: felt252 = 'Already initialized';
    const NOT_INITIALIZED: felt252 = 'Not initialized';
}

// Events
#[event]
#[derive(Drop, starknet::Event)]
enum Event {
    Transfer: Transfer,
    Approval: Approval,
    OwnershipTransferred: OwnershipTransferred,
    ItemProcessed: ItemProcessed,
}

#[derive(Drop, starknet::Event)]
struct Transfer {
    #[key]
    from: ContractAddress,
    #[key]
    to: ContractAddress,
    value: u256,
}

#[derive(Drop, starknet::Event)]
struct Approval {
    #[key]
    owner: ContractAddress,
    #[key]
    spender: ContractAddress,
    value: u256,
}

#[derive(Drop, starknet::Event)]
struct OwnershipTransferred {
    previous_owner: ContractAddress,
    new_owner: ContractAddress,
}

#[derive(Drop, starknet::Event)]
struct ItemProcessed {
    #[key]
    item_id: u256,
    processor: ContractAddress,
    value: u256,
    timestamp: u64,
}

// Storage struct
#[derive(Copy, Drop, Serde, starknet::Store)]
struct Item {
    id: u256,
    value: u256,
    owner: ContractAddress,
    active: bool,
}

#[derive(Copy, Drop, Serde, starknet::Store)]
struct Config {
    fee: u256,
    max_items: u256,
    is_paused: bool,
}

// Trait definition
trait IProcessor<TContractState> {
    fn process(ref self: TContractState, item_id: u256) -> u256;
    fn get_item(self: @TContractState, item_id: u256) -> Item;
    fn get_config(self: @TContractState) -> Config;
}

// Token trait
#[starknet::interface]
trait IToken<TContractState> {
    fn name(self: @TContractState) -> felt252;
    fn symbol(self: @TContractState) -> felt252;
    fn decimals(self: @TContractState) -> u8;
    fn total_supply(self: @TContractState) -> u256;
    fn balance_of(self: @TContractState, account: ContractAddress) -> u256;
    fn allowance(self: @TContractState, owner: ContractAddress, spender: ContractAddress) -> u256;
    fn transfer(ref self: TContractState, recipient: ContractAddress, amount: u256) -> bool;
    fn transfer_from(
        ref self: TContractState,
        sender: ContractAddress,
        recipient: ContractAddress,
        amount: u256
    ) -> bool;
    fn approve(ref self: TContractState, spender: ContractAddress, amount: u256) -> bool;
}

// Contract implementation
#[starknet::contract]
mod ProcessorContract {
    use super::{ContractAddress, Item, Config, Errors, Event, Transfer, ItemProcessed};
    use starknet::get_caller_address;
    use starknet::get_block_timestamp;
    use core::traits::Into;
    use core::option::OptionTrait;
    use core::array::ArrayTrait;

    // Storage
    #[storage]
    struct Storage {
        // Simple storage variables
        owner: ContractAddress,
        name: felt252,
        initialized: bool,
        total_items: u256,
        total_processed: u256,

        // Config
        config: Config,

        // Mappings
        balances: LegacyMap::<ContractAddress, u256>,
        items: LegacyMap::<u256, Item>,
        allowances: LegacyMap::<(ContractAddress, ContractAddress), u256>,
        item_count_by_owner: LegacyMap::<ContractAddress, u256>,
    }

    // Constructor
    #[constructor]
    fn constructor(
        ref self: ContractState,
        initial_owner: ContractAddress,
        name: felt252,
        fee: u256,
    ) {
        self.owner.write(initial_owner);
        self.name.write(name);
        self.initialized.write(true);

        let config = Config {
            fee: fee,
            max_items: super::MAX_ITEMS,
            is_paused: false,
        };
        self.config.write(config);

        self.total_items.write(0);
        self.total_processed.write(0);
    }

    // External functions
    #[external(v0)]
    impl ProcessorImpl of super::IProcessor<ContractState> {
        fn process(ref self: ContractState, item_id: u256) -> u256 {
            // Validate
            self._assert_not_paused();
            self._assert_item_exists(item_id);

            let caller = get_caller_address();
            let mut item = self.items.read(item_id);

            // Check ownership
            assert(item.owner == caller, Errors::UNAUTHORIZED);
            assert(item.active, 'Item not active');

            // Process
            let result = self._transform(item.value);

            // Update item
            item.value = result;
            self.items.write(item_id, item);

            // Update counters
            let current_processed = self.total_processed.read();
            self.total_processed.write(current_processed + 1);

            // Emit event
            self.emit(Event::ItemProcessed(ItemProcessed {
                item_id: item_id,
                processor: caller,
                value: result,
                timestamp: get_block_timestamp(),
            }));

            result
        }

        fn get_item(self: @ContractState, item_id: u256) -> Item {
            self.items.read(item_id)
        }

        fn get_config(self: @ContractState) -> Config {
            self.config.read()
        }
    }

    // Additional external functions
    #[external(v0)]
    fn create_item(ref self: ContractState, value: u256) -> u256 {
        self._assert_not_paused();

        let caller = get_caller_address();
        let config = self.config.read();

        // Check limits
        let current_total = self.total_items.read();
        assert(current_total < config.max_items, 'Max items reached');

        // Check fee
        let caller_balance = self.balances.read(caller);
        assert(caller_balance >= config.fee, Errors::INSUFFICIENT_BALANCE);

        // Deduct fee
        self.balances.write(caller, caller_balance - config.fee);

        // Create item
        let item_id = current_total + 1;
        let item = Item {
            id: item_id,
            value: value,
            owner: caller,
            active: true,
        };
        self.items.write(item_id, item);

        // Update counts
        self.total_items.write(item_id);
        let owner_count = self.item_count_by_owner.read(caller);
        self.item_count_by_owner.write(caller, owner_count + 1);

        item_id
    }

    #[external(v0)]
    fn transfer_item(
        ref self: ContractState,
        item_id: u256,
        to: ContractAddress
    ) {
        let caller = get_caller_address();
        let mut item = self.items.read(item_id);

        assert(item.owner == caller, Errors::UNAUTHORIZED);

        // Update ownership
        item.owner = to;
        self.items.write(item_id, item);

        // Update counts
        let from_count = self.item_count_by_owner.read(caller);
        self.item_count_by_owner.write(caller, from_count - 1);

        let to_count = self.item_count_by_owner.read(to);
        self.item_count_by_owner.write(to, to_count + 1);

        // Emit transfer event
        self.emit(Event::Transfer(Transfer {
            from: caller,
            to: to,
            value: item_id,
        }));
    }

    #[external(v0)]
    fn set_paused(ref self: ContractState, paused: bool) {
        self._assert_only_owner();
        let mut config = self.config.read();
        config.is_paused = paused;
        self.config.write(config);
    }

    #[external(v0)]
    fn transfer_ownership(ref self: ContractState, new_owner: ContractAddress) {
        self._assert_only_owner();

        let previous_owner = self.owner.read();
        self.owner.write(new_owner);

        self.emit(Event::OwnershipTransferred(super::OwnershipTransferred {
            previous_owner: previous_owner,
            new_owner: new_owner,
        }));
    }

    // View functions
    #[external(v0)]
    fn owner(self: @ContractState) -> ContractAddress {
        self.owner.read()
    }

    #[external(v0)]
    fn balance_of(self: @ContractState, account: ContractAddress) -> u256 {
        self.balances.read(account)
    }

    #[external(v0)]
    fn total_items(self: @ContractState) -> u256 {
        self.total_items.read()
    }

    // Internal functions
    #[generate_trait]
    impl InternalFunctions of InternalFunctionsTrait {
        fn _assert_only_owner(self: @ContractState) {
            let caller = get_caller_address();
            let owner = self.owner.read();
            assert(caller == owner, Errors::UNAUTHORIZED);
        }

        fn _assert_not_paused(self: @ContractState) {
            let config = self.config.read();
            assert(!config.is_paused, 'Contract is paused');
        }

        fn _assert_item_exists(self: @ContractState, item_id: u256) {
            let total = self.total_items.read();
            assert(item_id > 0 && item_id <= total, 'Item does not exist');
        }

        fn _transform(self: @ContractState, value: u256) -> u256 {
            // Simple transformation
            value * 2
        }
    }
}

// Helper functions
fn calculate_sum(a: u256, b: u256) -> u256 {
    a + b
}

fn transform(value: u256) -> u256 {
    if value == 0 {
        0
    } else if value < 10 {
        value * 2
    } else if value < 100 {
        value + 10
    } else {
        value
    }
}

// Tests
#[cfg(test)]
mod tests {
    use super::{calculate_sum, transform};

    #[test]
    fn test_calculate_sum() {
        let result = calculate_sum(5, 3);
        assert(result == 8, 'Sum should be 8');
    }

    #[test]
    fn test_transform_zero() {
        let result = transform(0);
        assert(result == 0, 'Transform of 0 should be 0');
    }

    #[test]
    fn test_transform_small() {
        let result = transform(5);
        assert(result == 10, 'Transform of 5 should be 10');
    }
}
