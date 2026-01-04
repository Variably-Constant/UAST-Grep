/**
 * Angular Test File for UAST-Grep
 * Tests: components, services, decorators, templates, directives
 */

// Imports
import { Component, OnInit, OnDestroy, Input, Output, EventEmitter } from '@angular/core';
import { Injectable, Inject } from '@angular/core';
import { Observable, Subject, BehaviorSubject, Subscription } from 'rxjs';
import { map, filter, takeUntil, debounceTime, distinctUntilChanged } from 'rxjs/operators';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { FormBuilder, FormGroup, Validators, AbstractControl } from '@angular/forms';
import { ActivatedRoute, Router, ParamMap } from '@angular/router';

// Constants
const MAX_ITEMS = 100;
const DEFAULT_NAME = 'UAST-Grep';
const API_URL = 'https://api.example.com';

// Interfaces
interface Person {
  id: number;
  name: string;
  age: number;
  email?: string;
  isActive: boolean;
}

interface ApiResponse<T> {
  data: T;
  total: number;
  page: number;
  pageSize: number;
}

// Enum
enum Status {
  Draft = 'draft',
  Published = 'published',
  Archived = 'archived'
}

// Injectable Service
@Injectable({
  providedIn: 'root'
})
export class DataService {
  private baseUrl = API_URL;
  private cache = new Map<string, any>();
  private refreshSubject = new BehaviorSubject<void>(undefined);

  constructor(private http: HttpClient) {}

  // HTTP GET
  getItems<T>(endpoint: string, params?: HttpParams): Observable<ApiResponse<T[]>> {
    const url = `${this.baseUrl}/${endpoint}`;
    return this.http.get<ApiResponse<T[]>>(url, { params }).pipe(
      map(response => {
        this.cache.set(endpoint, response);
        return response;
      })
    );
  }

  // HTTP POST
  createItem<T>(endpoint: string, data: T): Observable<T> {
    const url = `${this.baseUrl}/${endpoint}`;
    const headers = new HttpHeaders({ 'Content-Type': 'application/json' });
    return this.http.post<T>(url, data, { headers });
  }

  // HTTP PUT
  updateItem<T>(endpoint: string, id: number, data: Partial<T>): Observable<T> {
    const url = `${this.baseUrl}/${endpoint}/${id}`;
    return this.http.put<T>(url, data);
  }

  // HTTP DELETE
  deleteItem(endpoint: string, id: number): Observable<void> {
    const url = `${this.baseUrl}/${endpoint}/${id}`;
    return this.http.delete<void>(url);
  }

  // Get cached data
  getCached<T>(key: string): T | undefined {
    return this.cache.get(key);
  }

  // Trigger refresh
  refresh(): void {
    this.refreshSubject.next();
  }

  // Observable for refresh events
  get onRefresh$(): Observable<void> {
    return this.refreshSubject.asObservable();
  }
}

// Component
@Component({
  selector: 'app-test',
  template: `
    <!-- Template with Angular syntax -->
    <div class="container" [ngClass]="{'active': isActive, 'error': hasError}">
      <!-- Text interpolation -->
      <h1>{{ title }}</h1>
      <p>{{ message | uppercase }}</p>

      <!-- Property binding -->
      <img [src]="imageUrl" [alt]="imageAlt">
      <button [disabled]="isLoading">Submit</button>

      <!-- Event binding -->
      <button (click)="handleClick($event)">Click Me</button>
      <input (input)="onInput($event)" (keyup.enter)="onEnter()">

      <!-- Two-way binding -->
      <input [(ngModel)]="searchTerm" placeholder="Search...">

      <!-- Conditional rendering -->
      <div *ngIf="isVisible; else hiddenTemplate">
        Visible content
      </div>
      <ng-template #hiddenTemplate>
        Hidden content
      </ng-template>

      <!-- Switch -->
      <div [ngSwitch]="status">
        <span *ngSwitchCase="'draft'">Draft</span>
        <span *ngSwitchCase="'published'">Published</span>
        <span *ngSwitchDefault>Unknown</span>
      </div>

      <!-- List rendering -->
      <ul>
        <li *ngFor="let item of items; let i = index; trackBy: trackByFn">
          {{ i + 1 }}. {{ item.name }} - {{ item.age }}
        </li>
      </ul>

      <!-- Pipes -->
      <p>Date: {{ today | date:'fullDate' }}</p>
      <p>Currency: {{ price | currency:'USD':'symbol':'1.2-2' }}</p>
      <p>JSON: {{ data | json }}</p>

      <!-- Async pipe -->
      <div *ngIf="items$ | async as asyncItems">
        <span *ngFor="let item of asyncItems">{{ item.name }}</span>
      </div>

      <!-- Template reference variable -->
      <input #searchInput type="text">
      <button (click)="search(searchInput.value)">Search</button>

      <!-- Content projection -->
      <ng-content></ng-content>
      <ng-content select="[header]"></ng-content>

      <!-- Child component -->
      <app-child
        [data]="childData"
        [config]="childConfig"
        (onAction)="handleChildAction($event)"
        (onUpdate)="handleChildUpdate($event)">
      </app-child>

      <!-- Form -->
      <form [formGroup]="form" (ngSubmit)="onSubmit()">
        <div class="form-group">
          <label for="name">Name</label>
          <input id="name" formControlName="name" class="form-control">
          <div *ngIf="form.get('name')?.invalid && form.get('name')?.touched">
            <small class="error">Name is required</small>
          </div>
        </div>

        <div class="form-group">
          <label for="email">Email</label>
          <input id="email" formControlName="email" type="email">
        </div>

        <div formGroupName="address">
          <input formControlName="street" placeholder="Street">
          <input formControlName="city" placeholder="City">
        </div>

        <button type="submit" [disabled]="form.invalid">Submit</button>
      </form>
    </div>
  `,
  styles: [`
    .container {
      padding: 20px;
    }
    .active {
      border: 2px solid green;
    }
    .error {
      color: red;
    }
    .form-group {
      margin-bottom: 15px;
    }
  `]
})
export class TestComponent implements OnInit, OnDestroy {
  // Input properties
  @Input() title = DEFAULT_NAME;
  @Input() config: Record<string, any> = {};

  // Output events
  @Output() onAction = new EventEmitter<string>();
  @Output() onUpdate = new EventEmitter<Person>();

  // Component properties
  message = 'Hello from Angular!';
  isActive = true;
  isVisible = true;
  isLoading = false;
  hasError = false;
  status = Status.Draft;
  searchTerm = '';
  today = new Date();
  price = 99.99;
  imageUrl = 'https://example.com/image.png';
  imageAlt = 'Example image';

  items: Person[] = [];
  items$!: Observable<Person[]>;
  data: any = { key: 'value' };
  childData: any = {};
  childConfig: any = {};

  form!: FormGroup;

  private destroy$ = new Subject<void>();
  private subscription = new Subscription();

  constructor(
    private dataService: DataService,
    private fb: FormBuilder,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  ngOnInit(): void {
    // Initialize form
    this.form = this.fb.group({
      name: ['', [Validators.required, Validators.minLength(2)]],
      email: ['', [Validators.required, Validators.email]],
      address: this.fb.group({
        street: [''],
        city: ['']
      })
    });

    // Route params
    this.route.paramMap.pipe(
      takeUntil(this.destroy$)
    ).subscribe((params: ParamMap) => {
      const id = params.get('id');
      if (id) {
        this.loadItem(+id);
      }
    });

    // Query params
    this.route.queryParams.pipe(
      takeUntil(this.destroy$)
    ).subscribe(params => {
      this.searchTerm = params['search'] || '';
    });

    // Load items
    this.items$ = this.dataService.getItems<Person>('users').pipe(
      map(response => response.data)
    );

    // Subscribe to refresh
    this.subscription.add(
      this.dataService.onRefresh$.pipe(
        debounceTime(300),
        distinctUntilChanged()
      ).subscribe(() => this.refresh())
    );
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
    this.subscription.unsubscribe();
  }

  // Methods
  handleClick(event: MouseEvent): void {
    console.log('Clicked:', event);
    this.onAction.emit('click');
  }

  onInput(event: Event): void {
    const target = event.target as HTMLInputElement;
    this.searchTerm = target.value;
  }

  onEnter(): void {
    this.search(this.searchTerm);
  }

  search(term: string): void {
    this.router.navigate(['/search'], { queryParams: { q: term } });
  }

  handleChildAction(action: string): void {
    console.log('Child action:', action);
  }

  handleChildUpdate(person: Person): void {
    this.onUpdate.emit(person);
  }

  trackByFn(index: number, item: Person): number {
    return item.id;
  }

  onSubmit(): void {
    if (this.form.valid) {
      const formValue = this.form.value;
      this.dataService.createItem('users', formValue).subscribe({
        next: (result) => console.log('Created:', result),
        error: (err) => console.error('Error:', err)
      });
    }
  }

  private loadItem(id: number): void {
    // Load item logic
  }

  private refresh(): void {
    // Refresh logic
  }
}

// Child Component
@Component({
  selector: 'app-child',
  template: `
    <div class="child">
      <h2>{{ data?.title }}</h2>
      <button (click)="emitAction()">Action</button>
    </div>
  `
})
export class ChildComponent {
  @Input() data: any;
  @Input() config: any;
  @Output() onAction = new EventEmitter<string>();
  @Output() onUpdate = new EventEmitter<any>();

  emitAction(): void {
    this.onAction.emit('child-action');
  }
}

// Directive
import { Directive, ElementRef, HostListener, HostBinding } from '@angular/core';

@Directive({
  selector: '[appHighlight]'
})
export class HighlightDirective {
  @HostBinding('style.backgroundColor') backgroundColor = '';

  constructor(private el: ElementRef) {}

  @HostListener('mouseenter') onMouseEnter() {
    this.backgroundColor = 'yellow';
  }

  @HostListener('mouseleave') onMouseLeave() {
    this.backgroundColor = '';
  }
}

// Pipe
import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'truncate',
  pure: true
})
export class TruncatePipe implements PipeTransform {
  transform(value: string, limit = 50, trail = '...'): string {
    if (!value) return '';
    return value.length > limit ? value.substring(0, limit) + trail : value;
  }
}
